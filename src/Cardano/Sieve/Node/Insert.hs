{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer: write matched outputs into the SQLite schema.
--
-- 'openDatabase' installs the schema ('Cardano.Sieve.Schema.createSchema') and
-- 'writeSelected' persists the outputs that survived the sieve into @blocks@,
-- @outputs@, @unspent@ and @policies@. It uses 'sqlite-simple' — the same
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
  , StoredOutput (..)
  , SpentInput (..)
  , openDatabase
  , closeDatabase
  , applyBlock
  , rollbackAbove
  )
where

import Cardano.Sieve.Schema (createSchema)

import Control.Exception (onException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple
  ( Connection
  , Only (Only)
  , close
  , execute
  , execute_
  , open
  , query_
  , (:.) ((:.))
  )

-- | One matched output, with every field already serialised to the bytes the
-- schema stores. Produced by "Cardano.Sieve.Node.Filter"; consumed here.
data StoredOutput = StoredOutput
  { soOutputRef :: ByteString
  -- ^ Encoded output reference (transaction id ++ big-endian output index).
  -- The schema derives @transaction_id@ from this, so it is not stored again.
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
  , soReferenceScriptHash :: Maybe ByteString
  -- ^ Reference-script hash, if the output carries one.
  , soPolicyIds :: [ByteString]
  -- ^ Distinct policy ids of the assets in the value (ada excluded).
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
  -- ^ Index of this input within the spending transaction.
  }

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
openDatabase :: FilePath -> Int -> IO DbHandle
openDatabase path batchSize = do
  conn <- open path
  -- If preparing the freshly-opened connection throws, close it rather than
  -- leak the handle (the caller only gets to 'closeDatabase' a 'DbHandle' we return).
  prepare conn `onException` close conn
  pending <- newIORef 0
  pure (DbHandle conn (max 1 batchSize) pending)

-- | Commit the final partial batch (if any) and close the connection.
closeDatabase :: DbHandle -> IO ()
closeDatabase db = do
  flush db
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
applyBlock :: DbHandle -> Int64 -> ByteString -> [StoredOutput] -> [SpentInput] -> IO ()
applyBlock _ _ _ [] [] = pure ()
applyBlock (DbHandle conn batchSize pending) slot headerHash created spent = do
  n <- readIORef pending
  when (n == 0) $ execute_ conn "BEGIN TRANSACTION"
  unless (null created) $
    execute
      conn
      "INSERT OR IGNORE INTO blocks (slot_no, header_hash) VALUES (?, ?)"
      (slot, headerHash)
  mapM_ (insertOutput conn slot) created
  mapM_ (recordSpend conn slot) spent
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
    \(output_reference, address, value, datum_hash, reference_script_hash, created_slot) \
    \VALUES (?, ?, ?, ?, ?, ?)"
    ((soOutputRef o, soAddress o, soValue o, soDatumHash o) :. (soReferenceScriptHash o, slot))
  execute
    conn
    "INSERT OR IGNORE INTO unspent \
    \(output_reference, address, payment_credential, delegation_credential, \
    \value, datum_hash, reference_script_hash, created_slot) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( (soOutputRef o, soAddress o, soPayCred o, soDelegCred o)
        :. (soValue o, soDatumHash o, soReferenceScriptHash o, slot)
    )
  mapM_
    ( \pid ->
        execute
          conn
          "INSERT OR IGNORE INTO policies (output_reference, policy_id) VALUES (?, ?)"
          (soOutputRef o, pid)
    )
    (soPolicyIds o)

-- | Record one spend. Appends to @spends@ only when the consumed output is one
-- we track (the @WHERE EXISTS@ against @outputs@), and removes it from the live
-- @unspent@ set. The redeemer is not captured yet (left NULL). Untracked inputs
-- no-op on both statements.
recordSpend :: Connection -> Int64 -> SpentInput -> IO ()
recordSpend conn slot si = do
  execute
    conn
    "INSERT OR IGNORE INTO spends \
    \(output_reference, spending_transaction_id, spending_input_index, spent_slot) \
    \SELECT ?, ?, ?, ? \
    \WHERE EXISTS (SELECT 1 FROM outputs WHERE output_reference = ?)"
    (siConsumed si, siSpendingTxId si, siInputIndex si, slot, siConsumed si)
  execute
    conn
    "DELETE FROM unspent WHERE output_reference = ?"
    (Only (siConsumed si))

-- | Commit the currently open (partial) batch, if any.
flush :: DbHandle -> IO ()
flush (DbHandle conn _ pending) = do
  n <- readIORef pending
  when (n > 0) $ execute_ conn "COMMIT" >> writeIORef pending 0

-- | On a chain rollback, drop everything strictly newer than the rollback
-- point. 'Nothing' means roll back to genesis (delete everything). Commits any
-- open batch first so the deletes see a consistent database.
--
-- @policies@ is deleted before @outputs@ so the foreign key does not block the
-- @outputs@ delete. Restoring @unspent@ rows for outputs whose spend is rolled
-- back is deferred along with the @spends@ write path.
rollbackAbove :: DbHandle -> Maybe Int64 -> IO ()
rollbackAbove db@(DbHandle conn _ _) mSlot = do
  flush db
  case mSlot of
    Nothing ->
      mapM_
        (execute_ conn)
        [ "DELETE FROM policies"
        , "DELETE FROM unspent"
        , "DELETE FROM spends"
        , "DELETE FROM outputs"
        , "DELETE FROM blocks"
        ]
    Just slot -> do
      execute
        conn
        "DELETE FROM policies WHERE output_reference IN \
        \(SELECT output_reference FROM outputs WHERE created_slot > ?)"
        (Only slot)
      execute conn "DELETE FROM unspent WHERE created_slot > ?" (Only slot)
      execute conn "DELETE FROM spends WHERE spent_slot > ?" (Only slot)
      execute conn "DELETE FROM outputs WHERE created_slot > ?" (Only slot)
      execute conn "DELETE FROM blocks WHERE slot_no > ?" (Only slot)
