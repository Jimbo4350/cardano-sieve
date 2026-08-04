{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer: write matched outputs into the SQLite schema.
--
-- 'openDatabase' installs the schema ('Cardano.Sieve.Schema.createSchema') and
-- 'writeSelected' persists the outputs that survived the sieve into @blocks@,
-- @outputs@, @unspent@ and @policies@ (interning policy hashes through
-- @policy_ids@ as it goes). It uses 'sqlite-simple' — the same
-- library kupo uses — so throughput/memory comparisons stay apples to apples.
--
-- This module is deliberately SQLite-only: it knows nothing about
-- @cardano-api@. The decode stage ("Cardano.Sieve.Node.Filter") turns a block
-- into ready-to-store 'StoredOutput's (all fields already serialised to bytes),
-- and this module just writes them.
--
-- Writes are /batched/: inserts accumulate inside a single open transaction and
-- only COMMIT every N outputs, amortising SQLite's per-commit @fsync@ over the
-- batch. The open transaction is SQLite's own buffer, so this needs no
-- application-level queue (ADR-020 Decision 1). The final partial batch is
-- committed by 'closeDatabase'; run it via 'Control.Exception.finally' so it
-- also fires when the caller is torn down by an async exception.
--
-- Not persisted yet: datum/script /preimages/ (@binary_data@ / @scripts@) — we
-- store the hashes on the output rows but not the bodies — and /spends/ (the
-- @spends@ table and delete-from-@unspent@ on consumption). Both are the next
-- write-path cut.
module Cardano.Sieve.Node.Insert
  ( DbHandle
  , DatumType (..)
  , RedeemerCapture (..)
  , StoredOutput (..)
  , SpentInput (..)
  , Preimages (..)
  , openDatabase
  , closeDatabase
  , applyBlock
  , rollbackAbove
  , buildIndexesOn
  , installIndexes
  , busyTimeoutMs
  , flushBatch
  , reconcileSelectors
  , resumePoints
  , SelectorMismatch (..)
  , StoredSelectorUnparseable (..)
  )
where

import Cardano.Sieve.Schema (createSchema, installDeferredIndexes)
import Cardano.Sieve.Selector
  ( BootstrapFilter (IncludeBootstrap)
  , Selector (SelectAll)
  , selectorFromText
  , selectorToText
  )

import Control.Exception (Exception, bracket, onException, throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Database.SQLite.Simple
  ( Connection
  , Only (Only)
  , Query
  , close
  , execute
  , execute_
  , open
  , query
  , query_
  , (:.) ((:.))
  )

-- | How an output supplied its datum: written out in full on the output itself,
-- or referenced only by hash. Kupo reports this as @datum_type@, and it is the
-- one thing a datum hash alone cannot tell you — with @DatumByHash@ the body may
-- not exist anywhere yet, whereas @DatumInline@ guarantees it was on chain with
-- the output.
data DatumType = DatumInline | DatumByHash
  deriving (Eq, Show)

-- | 'DatumType' as the schema stores it. Kept next to the type so the mapping is
-- in one place; 0 and 1 rather than text to keep the column narrow.
datumTypeToInt :: DatumType -> Int64
datumTypeToInt = \case
  DatumByHash -> 0
  DatumInline -> 1

-- | One matched output, with every field already serialised to the bytes the
-- schema stores. Produced by "Cardano.Sieve.Node.Filter"; consumed here.
data StoredOutput = StoredOutput
  { soOutputRef :: ByteString
  -- ^ Encoded output reference (transaction id ++ big-endian output index).
  -- The schema derives @transaction_id@ from this, so it is not stored again.
  , soTransactionIndex :: Int64
  -- ^ Position of the producing transaction within its block. Stored because
  -- kupo reports it, and because kupo's result ordering is
  -- @(created_slot, transaction_index, output_index)@ — nothing else recovers it.
  , soAddress :: ByteString
  -- ^ Raw bytes of the output's address.
  , soPayCred :: Maybe ByteString
  -- ^ 28-byte payment credential hash (Nothing for Byron addresses).
  , soDelegCred :: Maybe ByteString
  -- ^ 28-byte delegation credential hash (Nothing unless a base address).
  , soValue :: ByteString
  -- ^ Serialised value (see "Cardano.Sieve.Node.Filter" for the encoding).
  , soDatumHash :: Maybe ByteString
  -- ^ Datum hash, if the output carries a datum (hash or inline).
  , soDatumType :: Maybe DatumType
  -- ^ Which of the two, when there is a datum at all. 'Nothing' exactly when
  -- 'soDatumHash' is 'Nothing'.
  , soReferenceScriptHash :: Maybe ByteString
  -- ^ Reference-script hash, if the output carries one.
  , soAssets :: [(ByteString, ByteString)]
  -- ^ Distinct (policy id, asset name) pairs of the assets in the value (ada
  -- excluded); both raw bytes, asset name possibly empty.
  }

-- | One consumed transaction input, for recording a spend; fields already
-- serialised. @siConsumed@ is the encoded reference of the output being spent
-- — the same encoding as @soOutputRef@ — so it can be matched against @outputs@.
data SpentInput = SpentInput
  { siConsumed :: ByteString
  -- ^ Encoded output reference of the consumed output.
  , siSpendingTxId :: ByteString
  -- ^ Raw id of the transaction doing the spending.
  , siInputIndex :: Int64
  -- ^ Index of this input within the spending transaction, in the ledger's
  -- (sorted) input order.
  , siRedeemer :: Maybe ByteString
  -- ^ The redeemer that authorised this spend, when there is one and capture is
  -- enabled. 'Nothing' both for a non-script spend and when capture is off, which
  -- the schema cannot distinguish — see 'RedeemerCapture'.
  }

-- | Whether to pull spend redeemers out of the witness set and store them.
--
-- Opt-in because redeemers are the heavy bytes on the spend path, and everything
-- else about a spend (which transaction, which slot, which input) is cheap and
-- always recorded. This is the switch the storage design called for.
data RedeemerCapture = CaptureRedeemers | SkipRedeemers
  deriving (Eq, Show)

-- | Datum and script /preimages/ from one block: the bodies behind the hashes
-- stored on output rows, for the deduplicated @binary_data@ and @scripts@ tables.
-- Produced by "Cardano.Sieve.Node.Decode"; written by 'applyBlock'.
--
-- Block-scoped rather than output-scoped because that is where the data lives: a
-- datum referenced by hash from an output is supplied in the /witness set/ of a
-- transaction, and that is frequently a later transaction than the one that
-- created the output.
data Preimages = Preimages
  { pmDatums :: [(ByteString, ByteString)]
  -- ^ (datum hash, datum bytes).
  , pmScripts :: [(ByteString, ByteString)]
  -- ^ (script hash, script bytes).
  }

instance Semigroup Preimages where
  a <> b = Preimages (pmDatums a <> pmDatums b) (pmScripts a <> pmScripts b)

instance Monoid Preimages where
  mempty = Preimages [] []

-- | A handle to the SQLite database; output writes are batched.
data DbHandle = DbHandle
  { dbConn :: Connection
  , dbBatchSize :: Int
  -- ^ COMMIT once this many outputs have accumulated in the open transaction.
  , dbUncommittedRows :: IORef Int
  -- ^ Outputs written into the open transaction but not yet committed; climbs
  -- to 'dbBatchSize', then a COMMIT resets it to 0 (also the open-transaction
  -- flag: 0 = no transaction open).
  }

-- | Open the database, prepare it (pragmas + schema) and return a batched
-- 'DbHandle'. Pair every 'openDatabase' with 'closeDatabase' — via
-- 'Control.Exception.finally' — so the final partial batch is always committed.
-- | How long a connection waits for a contended lock before giving up, in
-- milliseconds. Shared by the writer here and the query server's readers so the
-- two agree.
busyTimeoutMs :: Query
busyTimeoutMs = "5000"

openDatabase :: FilePath -> Int -> IO DbHandle
openDatabase path batchSize = do
  conn <- open path
  -- If preparing the freshly-opened connection throws, close it rather than
  -- leak the handle (the caller only gets to 'closeDatabase' a 'DbHandle' we return).
  prepare conn `onException` close conn
  pending <- newIORef 0
  pure (DbHandle conn (max 1 batchSize) pending)

-- | Install the deferred secondary indexes on an already-open handle, committing
-- the open batch first. Fired once on reaching the chain tip — bulk catch-up
-- runs index-free to keep writes cheap (see "Cardano.Sieve.Schema").
buildIndexesOn :: DbHandle -> IO ()
buildIndexesOn db = flushBatch db >> installDeferredIndexes (dbConn db)

-- | Open an existing database, install the deferred query indexes, and close.
-- The @--build-indexes@ one-shot, for databases that never reach live tip (e.g.
-- a bounded @--until@ sync or the benchmark).
installIndexes :: FilePath -> IO ()
installIndexes path = bracket (openDatabase path 1) closeDatabase buildIndexesOn

-- | Commit the final partial batch (if any) and close the connection.
closeDatabase :: DbHandle -> IO ()
closeDatabase db = do
  flushBatch db
  close (dbConn db)

-- | Ready a freshly-opened connection for batched writes: set the session
-- PRAGMAs, then create the schema.
--
-- @journal_mode=WAL@ lets the writer commit without blocking readers and keeps
-- each commit cheap; @synchronous=NORMAL@ replaces the per-commit @fsync@ with
-- one at each WAL checkpoint. @foreign_keys=ON@ enforces the @policies@ →
-- @outputs@ reference the schema declares. Together with batching, WAL +
-- NORMAL is where the write throughput comes from.
--
-- The two mode PRAGMAs go through 'query_' rather than 'execute_' because
-- @PRAGMA journal_mode@ returns a row (the mode it settled on) and 'execute_'
-- rejects statements that produce output.
prepare :: Connection -> IO ()
prepare conn = do
  mapM_
    (\q -> () <$ (query_ conn q :: IO [Only Text]))
    [ "PRAGMA journal_mode=WAL"
    , "PRAGMA synchronous=NORMAL"
    ]
  -- Wait for a contended lock instead of failing on it. SQLite's default is 0 ms
  -- — return SQLITE_BUSY immediately — which is fine while the writer has the
  -- file to itself but not once a query server shares the process
  -- (@--serve@ alongside @--socket-path@): WAL keeps ordinary reads clear of the
  -- writer, yet the brief exclusive moments still collide, and with no timeout
  -- the loser errors rather than waiting a few milliseconds.
  --
  -- Read back separately from the pragmas above because it answers with an
  -- INTEGER where @journal_mode@ answers with TEXT.
  () <$ (query_ conn ("PRAGMA busy_timeout=" <> busyTimeoutMs) :: IO [Only Int])
  execute_ conn "PRAGMA foreign_keys=ON"
  createSchema conn

-- | Apply a block's effect: persist the selected outputs it created and record
-- the spends of any tracked outputs its transactions consumed, in one batched
-- unit. Created outputs go into @outputs@ + @unspent@ (+ a @policies@ row per
-- policy) under @created_slot@; each spend appends to @spends@ (only when the
-- consumed output is one we track) and removes it from the live @unspent@ set.
-- A no-op only for an empty block.
--
-- Created outputs are written before spends are recorded, so an output created
-- and spent within the same block is visible to the spend's existence check.
--
-- Opens a transaction lazily and commits once 'dbBatchSize' rows have
-- accumulated. Inserts are @INSERT OR IGNORE@ — idempotent across re-syncs, and
-- avoiding the @INSERT OR REPLACE@ delete that would trip the @policies@
-- foreign key.
applyBlock
  :: DbHandle -> Int64 -> ByteString -> [StoredOutput] -> [SpentInput] -> Preimages -> IO ()
applyBlock _ _ _ [] [] _ = pure ()
applyBlock
  DbHandle
    { dbConn = conn
    , dbBatchSize = batchSize
    , dbUncommittedRows = pending
    }
  slot
  headerHash
  created
  spent
  preimages = do
    n <- readIORef pending
    when (n == 0) $ execute_ conn "BEGIN TRANSACTION"
    -- Where we are on the chain, for resuming after a restart. Written for every
    -- APPLIED block rather than every block seen: the empty-block fast path above
    -- returns before this, so a quiet stretch leaves no checkpoint and a resume
    -- rewinds to the last block that actually mattered. Correct — replaying
    -- blocks is idempotent, every insert here is OR IGNORE — and it keeps the
    -- fast path free.
    --
    -- Rides the caller's open transaction, so the checkpoint and the rows it
    -- vouches for commit together. Never one without the other.
    execute
      conn
      "INSERT OR IGNORE INTO checkpoints (slot_no, header_hash) VALUES (?, ?)"
      (slot, headerHash)
    unless (null created) $
      execute
        conn
        "INSERT OR IGNORE INTO blocks (slot_no, header_hash) VALUES (?, ?)"
        (slot, headerHash)
    mapM_ (insertOutput conn slot) created
    mapM_ (recordSpend conn slot) spent
    -- Preimages are gathered from the WHOLE block, so gate them on the block
    -- being relevant to the configured selectors — otherwise a narrow selector
    -- drags in every datum and script on the chain. kupo does the same, and says
    -- why: "a best-effort at not storing all the garbage of the world".
    --
    -- kupo's gate is "produced a tracked output OR spent a tracked input"; ours is
    -- the first half only. Detecting the second would cost a lookup per input on
    -- the sync hot path, and under a wildcard selector (how the benchmark runs)
    -- any block with transactions produces tracked outputs, so the two coincide.
    -- Under a narrow selector ours stores strictly less, which is the safe
    -- direction. Verified by comparing the resulting row counts against kupo.
    unless (null created) $ do
      mapM_ (insertPreimage conn "binary_data" "datum_hash" "datum") (pmDatums preimages)
      mapM_ (insertPreimage conn "scripts" "script_hash" "script") (pmScripts preimages)
    let n' = n + length created + length spent
    if n' >= batchSize
      then execute_ conn "COMMIT" >> writeIORef pending 0
      else writeIORef pending n'

-- | Write one matched output. @outputs@ is inserted before @policies@ so the
-- foreign key is satisfied within the transaction.
insertOutput :: Connection -> Int64 -> StoredOutput -> IO ()
insertOutput conn slot o = do
  execute
    conn
    "INSERT OR IGNORE INTO outputs \
    \(output_reference, transaction_index, address, payment_credential, \
    \delegation_credential, value, datum_hash, datum_type, reference_script_hash, \
    \created_slot) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( (soOutputRef o, soTransactionIndex o, soAddress o, soPayCred o, soDelegCred o)
        :. ( soValue o
           , soDatumHash o
           , datumTypeToInt <$> soDatumType o
           , soReferenceScriptHash o
           , slot
           )
    )
  execute
    conn
    "INSERT OR IGNORE INTO unspent \
    \(output_reference, transaction_index, address, payment_credential, \
    \delegation_credential, value, datum_hash, datum_type, reference_script_hash, \
    \created_slot) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( (soOutputRef o, soTransactionIndex o, soAddress o, soPayCred o, soDelegCred o)
        :. ( soValue o
           , soDatumHash o
           , datumTypeToInt <$> soDatumType o
           , soReferenceScriptHash o
           , slot
           )
    )
  mapM_
    ( \(pid, name) -> do
        num <- policyNumOf conn pid
        execute
          conn
          "INSERT OR IGNORE INTO policies (output_reference, policy_num, asset_name, created_slot) \
          \VALUES (?, ?, ?, ?)"
          (soOutputRef o, num, name, slot)
    )
    (soAssets o)

-- | Resolve a policy hash to its @policy_ids@ surrogate, inserting the
-- dictionary row the first time that policy is seen.
--
-- Two statements, run once per asset row. The dictionary write joins the
-- caller's open transaction, so a policy's surrogate and the @policies@ rows
-- referencing it commit together.
--
-- Two because @INSERT OR IGNORE@ reports nothing when it ignores, and
-- @last_insert_rowid()@ is only meaningful when a row was actually inserted —
-- which it is not for all but the first sighting of each policy. The number has
-- to be read back either way.
--
-- == Faster spellings exist and were rejected
--
-- Measured on @origin..2,000,000@, against this as the 100% mark
-- (@bench\/run-flush-check.sh@, 2026-08-04):
--
-- +---------------------------------------------+---------+
-- | in-memory @Map@ of hash to surrogate         | -13.9%  |
-- | the same two statements, prepared once       | -9.8%   |
-- | one @INSERT … ON CONFLICT … RETURNING@       | +34.4%  |
-- +---------------------------------------------+---------+
--
-- The @Map@ was fastest and is gone deliberately: it was application-level
-- memoisation living in 'DbHandle' beside a connection and a row counter, and it
-- forced @policy_ids@ to stay append-only across rollbacks so it could not go
-- stale. Prepared statements have the same problem in smaller form — they are
-- one table's access pattern held in a general handle.
--
-- The single-statement @RETURNING@ version looks like the obvious win and is by
-- far the worst. @ON CONFLICT DO NOTHING@ cannot be used, because @RETURNING@
-- only reports rows the statement acted on, so a repeat policy returns nothing;
-- the workaround is @DO UPDATE SET policy_id = excluded.policy_id@, which
-- REWRITES the row every time. That is roughly three million writes to a
-- 1,613-row table, and no amount of halving the statement count pays for it.
--
-- What remains is the slowest and the plainest. That was the call: two ordinary
-- statements, no cache, nothing about policies in 'DbHandle'.
policyNumOf :: Connection -> ByteString -> IO Int64
policyNumOf conn pid = do
  execute conn "INSERT OR IGNORE INTO policy_ids (policy_id) VALUES (?)" (Only pid)
  rows <- query conn "SELECT policy_num FROM policy_ids WHERE policy_id = ?" (Only pid)
  case rows of
    Only num : _ -> pure num
    [] -> error "policyNumOf: policy_ids row absent immediately after INSERT OR IGNORE"

-- | Store one preimage, keyed by its hash. @INSERT OR IGNORE@ does the dedup: the
-- same datum or script recurs across many transactions, and the hash is the
-- primary key, so repeats cost a failed index probe rather than a row.
--
-- The table and column names are supplied by the caller rather than duplicating
-- this function per table; they are compile-time literals here, never user input.
insertPreimage :: Connection -> Query -> Query -> Query -> (ByteString, ByteString) -> IO ()
insertPreimage conn table hashCol bodyCol (h, body) =
  execute
    conn
    ("INSERT OR IGNORE INTO " <> table <> " (" <> hashCol <> ", " <> bodyCol <> ") VALUES (?, ?)")
    (h, body)

-- | Record one spend. Appends to @spends@ only when the consumed output is one
-- we track (the @WHERE EXISTS@ against @outputs@), and removes it from the live
-- @unspent@ set. The redeemer is whatever the decode stage captured — NULL unless
-- 'CaptureRedeemers' was asked for. Untracked inputs no-op on both statements.
recordSpend :: Connection -> Int64 -> SpentInput -> IO ()
recordSpend conn slot si = do
  execute
    conn
    "INSERT OR IGNORE INTO spends \
    \(output_reference, spending_transaction_id, spending_input_index, spent_slot, \
    \redeemer) \
    \SELECT ?, ?, ?, ?, ? \
    \WHERE EXISTS (SELECT 1 FROM outputs WHERE output_reference = ?)"
    ( (siConsumed si, siSpendingTxId si, siInputIndex si)
        :. (slot, siRedeemer si, siConsumed si)
    )
  execute
    conn
    "DELETE FROM unspent WHERE output_reference = ?"
    (Only (siConsumed si))

-- | Commit the currently open (partial) batch, if any. A no-op when nothing is
-- pending, so it is cheap to call speculatively — which is what the idle flush in
-- "Cardano.Sieve.Node.Fetch" does on every empty pipeline.
flushBatch :: DbHandle -> IO ()
flushBatch DbHandle{dbConn = conn, dbUncommittedRows = pending} = do
  n <- readIORef pending
  when (n > 0) $ execute_ conn "COMMIT" >> writeIORef pending 0

-- | On a chain rollback, drop everything strictly newer than the rollback
-- point. 'Nothing' means roll back to genesis (delete everything). Commits any
-- open batch first so the deletes see a consistent database.
--
-- @policies@ is deleted before @outputs@ so the foreign key does not block the
-- @outputs@ delete.
--
-- Undoing a spend has to return its output to @unspent@, or the invariant the
-- three-table split rests on — an output is in @unspent@ exactly when it has no
-- @spends@ row — breaks silently and permanently: the output stays in @outputs@
-- with no spend recorded, yet no @?unspent@ query can ever see it again. kupo
-- keeps one @inputs@ table with a nullable @spent_at@, so for it this is a single
-- @UPDATE inputs SET spent_at = NULL WHERE spent_at > ?@; our split needs a
-- re-insert from @outputs@.
--
-- @policy_ids@ is deliberately /not/ pruned. It is a pure interning dictionary
-- with no slot column, keeping it append-only means surrogates never change and
-- the 'dbPolicyNums' cache never needs invalidating, and the orphans a rollback
-- leaves behind are bounded by the number of distinct policies ever seen.
rollbackAbove :: DbHandle -> Maybe Int64 -> IO ()
rollbackAbove db@DbHandle{dbConn = conn} mSlot = do
  flushBatch db
  case mSlot of
    Nothing ->
      mapM_
        (execute_ conn)
        [ "DELETE FROM policies"
        , "DELETE FROM unspent"
        , "DELETE FROM spends"
        , "DELETE FROM outputs"
        , "DELETE FROM blocks"
        , "DELETE FROM checkpoints"
        ]
    Just slot -> do
      -- Restore first: the @spends@ rows about to be deleted are what identifies
      -- which outputs to bring back, so this cannot run after them.
      --
      -- The @created_slot <= ?@ guard is what makes the result independent of
      -- statement order rather than reliant on it. Without it, an output created
      -- AND spent above the rollback point — which must disappear entirely —
      -- would be reinstated here and then survive if this ran after the
      -- @unspent@ delete. kupo orders the equivalent pair the same way
      -- (@rollbackQryUpdateInputs@ before @rollbackQryDeleteInputs@).
      --
      -- @spends(spent_slot)@ is indexed, and rollbacks only occur near the tip,
      -- by which point the deferred indexes are built — so the subquery is a
      -- range scan, not a table scan.
      execute
        conn
        "INSERT OR IGNORE INTO unspent \
        \(output_reference, transaction_index, address, payment_credential, \
        \delegation_credential, value, datum_hash, datum_type, \
        \reference_script_hash, created_slot) \
        \SELECT output_reference, transaction_index, address, payment_credential, \
        \delegation_credential, value, datum_hash, datum_type, \
        \reference_script_hash, created_slot \
        \FROM outputs \
        \WHERE created_slot <= ? \
        \AND output_reference IN (SELECT output_reference FROM spends WHERE spent_slot > ?)"
        (slot, slot)
      execute conn "DELETE FROM policies WHERE created_slot > ?" (Only slot)
      execute conn "DELETE FROM unspent WHERE created_slot > ?" (Only slot)
      execute conn "DELETE FROM spends WHERE spent_slot > ?" (Only slot)
      execute conn "DELETE FROM outputs WHERE created_slot > ?" (Only slot)
      execute conn "DELETE FROM blocks WHERE slot_no > ?" (Only slot)
      -- Must go with the rest: a checkpoint above the rollback point names a
      -- block that is no longer on our chain, and offering it to the node on the
      -- next restart would resume from a fork.
      execute conn "DELETE FROM checkpoints WHERE slot_no > ?" (Only slot)

-- * Selector bookkeeping

-- | The configured selectors do not match the ones this database was built with.
--
-- Fatal, deliberately, and in BOTH directions — which is worth spelling out
-- because only one direction looks dangerous at first glance:
--
--   * __Removing__ a selector leaves the database with no /new/ data for it,
--     while the rows it already produced stay. Queries for it silently miss
--     recent matches.
--   * __Adding__ one leaves the database with no /historical/ data for it.
--     Queries for it silently miss old matches.
--
-- Both leave the database incomplete with respect to the patterns it claims to
-- serve, and neither announces itself at query time — the result is simply
-- short. kupo refuses both for the same reason, and repairs the add case by
-- rolling the indexer back to re-index; sieve has no such mechanism, so it
-- refuses and says what to do instead.
data SelectorMismatch = SelectorMismatch
  { smStored :: [Selector]
  , smConfigured :: [Selector]
  }

instance Show SelectorMismatch where
  show (SelectorMismatch stored configured) =
    unlines
      ( [ "this database was indexed with different selectors."
        , ""
        , "  stored:     " <> render stored
        , "  configured: " <> render configured
        , ""
        , "Indexing on would leave it incomplete for the selectors it claims to"
        , "serve: a removed selector stops gaining new matches, an added one has"
        , "no history. Neither shows up at query time — results are just short."
        , ""
        , "Either use the stored selectors, or index into a fresh --database."
        ]
      )
   where
    render = \case
      [] -> "(none)"
      xs -> unwords (map show (sort (map selectorToText xs)))

instance Exception SelectorMismatch

-- | A row in @patterns@ that 'selectorFromText' cannot read back.
--
-- Only reachable through schema drift or a hand-edited database, since every row
-- is written by 'selectorToText' and the pair round-trips. Fatal rather than
-- skipped: a selector we cannot parse is one we cannot honour, and silently
-- dropping it would turn this check into a way to LOSE a selector.
newtype StoredSelectorUnparseable = StoredSelectorUnparseable Text

instance Show StoredSelectorUnparseable where
  show (StoredSelectorUnparseable t) =
    "patterns table holds a selector this build cannot parse: " <> show t

instance Exception StoredSelectorUnparseable

-- | Reconcile the selectors given on the command line against the ones this
-- database was indexed with, and return the set to index with.
--
-- First run writes the configured set and returns it. Later runs must match it
-- exactly or throw 'SelectorMismatch'. Passing none adopts what is stored, which
-- is what lets a restart carry on without repeating every @--select@.
--
-- Runs inside the caller's transaction discipline: it commits nothing itself,
-- and the write it may perform is picked up by the next 'flushBatch'.
reconcileSelectors :: DbHandle -> [Selector] -> IO [Selector]
reconcileSelectors DbHandle{dbConn = conn} configured = do
  rows <- query_ conn "SELECT selector FROM patterns"
  stored <- traverse parseStored [t | Only t <- rows]
  case (stored, configured) of
    -- Nothing stored and nothing asked for: match everything, and record that,
    -- so the next run adopts it rather than re-deciding. The default lives here
    -- rather than in the option parser because it is a value that has to be
    -- PERSISTED, and this is the only place that both chooses and writes it.
    ([], []) -> defaulted <$ mapM_ insertSelector defaulted
    ([], _) -> configured <$ mapM_ insertSelector configured
    -- Asked for nothing, so use what the database was built with. This is what
    -- makes a bare restart work without repeating every --select.
    (_, []) -> pure stored
    _
      -- Compared on the canonical text, not the ADT: that text is what the table
      -- actually holds, 'Selector' has no 'Ord', and the codec round-trips, so
      -- text equality and selector equality are the same question.
      | Set.fromList (map selectorToText stored)
          == Set.fromList (map selectorToText configured) ->
          pure stored
      | otherwise -> throwIO (SelectorMismatch stored configured)
 where
  defaulted = [SelectAll IncludeBootstrap]
  parseStored t = either (const (throwIO (StoredSelectorUnparseable t))) pure (selectorFromText t)
  insertSelector s =
    execute
      conn
      "INSERT OR IGNORE INTO patterns (selector) VALUES (?)"
      (Only (selectorToText s))

-- * Resume

-- | Points to offer the node when resuming, newest first.
--
-- @MsgFindIntersect@ takes a LIST and the node replies with the newest point it
-- recognises, so this is not "where we stopped" but "everywhere we might
-- plausibly rejoin". One point would be enough only if the node's chain still
-- contained it; if the node rolled back past it while we were down, a single
-- point fails to intersect and there is nothing to fall back to but genesis.
--
-- The spacing is exponential — newest, then 1, 2, 4, 8 … rows further back —
-- so a shallow rollback rejoins within a block or two of where we stopped, and
-- an implausibly deep one still finds something without carrying every
-- checkpoint over the wire. kupo builds its ladder the same way.
--
-- Capped at 'resumePointCount' entries. Genesis is not included; the caller
-- appends it as the last resort.
resumePoints :: DbHandle -> IO [(Int64, ByteString)]
resumePoints DbHandle{dbConn = conn} = do
  rows <- query_ conn "SELECT slot_no, header_hash FROM checkpoints ORDER BY slot_no DESC"
  pure (withOldest rows (pick 0 1 rows))
 where
  -- Take row 0, then step 1, 2, 4, 8 … forward through the descending list.
  pick _ _ [] = []
  pick taken step (x : xs)
    | taken >= resumePointCount = []
    | otherwise = x : pick (taken + 1) (step * 2) (drop (step - 1) xs)

  -- Always end on the oldest checkpoint we hold. The exponential steps overshoot
  -- the end of the list, so without this the deepest point on offer is an
  -- arbitrary one partway back, and anything older falls all the way to genesis
  -- — re-reading the whole chain to recover from a rollback we had the data to
  -- survive.
  withOldest rows picked = case (reverse rows, reverse picked) of
    (oldest : _, deepest : _) | fst oldest /= fst deepest -> picked <> [oldest]
    _ -> picked

-- | How many points to offer. Enough to span a deep rollback at exponential
-- spacing (the 20th reaches ~500,000 checkpoints back) without making the
-- intersect message large.
resumePointCount :: Int
resumePointCount = 20
