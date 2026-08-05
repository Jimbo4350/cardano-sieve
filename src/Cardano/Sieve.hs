{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | Executable entry point. Parses command-line options, follows a local node's
-- chain, sieves each block's outputs against the configured selectors, and
-- writes the matches into SQLite, printing each block number and its match
-- count as it is rolled forward.
--
-- Connect over node-to-client ChainSync, decode each block, run the sieve, and
-- persist matched outputs (ADR-020 and the architecture notes at the bottom of
-- this module). Selectors come from repeatable @--select@ options, each parsed
-- by 'Cardano.Sieve.Selector.selectorFromText'; with none given, every output is
-- matched.
module Cardano.Sieve
  ( sieve
  )
where

import Cardano.Api
  ( BlockHeader
  , ChainPoint (ChainPoint, ChainPointAtGenesis)
  , File (File)
  , Hash
  , NetworkId (Testnet)
  , NetworkMagic (NetworkMagic)
  , SocketPath
  , deserialiseFromRawBytesHex
  )

import Cardano.Server.Http (runServer)
import Cardano.Sieve.Node.Fetch (fetch, fetchBounded)
import Cardano.Sieve.Node.Insert
  ( Durability (Durable, UnsafeBulk)
  , RedeemerCapture (CaptureRedeemers, SkipRedeemers)
  , closeDatabase
  , installIndexes
  , openDatabase
  )
import Cardano.Sieve.Selector (Selector, selectorFromText)
import Cardano.Slotting.Slot (SlotNo (SlotNo))

import Control.Applicative (many, optional)
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (race_)
import Control.Exception (AsyncException (UserInterrupt), bracket, throwTo)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative
  ( Parser
  , ParserInfo
  , auto
  , eitherReader
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , showDefault
  , showDefaultWith
  , strOption
  , switch
  , value
  , (<**>)
  )
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigTERM)
import Text.Read (readMaybe)

-- | What the process was asked to do.
--
-- The three are mutually exclusive and need /different/ inputs, which is what a
-- flat record of options could not express. Encoding the choice as
-- @servePort :: Maybe Int@ plus a @buildIndexes :: Bool@ guard made every mode
-- carry every field, so the modes that never open a node connection still had to
-- be handed one: @bench\/run-query-compare.sh@ passed @--socket-path
-- \/tmp\/unused.sock@ to @--build-indexes@ purely to satisfy the parser, and
-- @--serve@ demanded a @--testnet-magic@ it ignores.
--
-- Note that the two 'Maybe's that remain are genuine absences, not modes hiding
-- in a missing value the way @servePort@ was: 'syUntil' absent means no upper
-- bound, and the port on 'Sync' absent means index without also serving.
data Command
  = -- | Follow the chain and index it into @--database@, optionally serving the
    -- read query API from the same process on the given port.
    --
    -- __Serving while syncing is only as fresh as the last commit.__ Batches
    -- currently commit on a row count ('syBatchSize'), so at the tip — where
    -- blocks carry few matched rows — an open transaction can sit unflushed
    -- indefinitely and queries answer from stale data with no indication.
    -- ADR-020 Decision 2 (flush on idle, using @CollectResponse@'s non-blocking
    -- peek) is what makes this sound; until it lands, prefer a port here only
    -- for a bounded @--until@ run, where the final commit happens on exit.
    Sync SyncOptions (Maybe Int)
  | -- | Install the deferred query indexes on @--database@ and exit. Run once
    -- after an initial sync that never reached the tip (a @--until@ run does not
    -- build them).
    BuildIndexes
  | -- | Serve the read query API on this port against an existing @--database@,
    -- without connecting to a node.
    --
    -- Deliberately carries no socket path: it needs none today, and
    -- @GET \/metadata\/{slot-no}@ should add one to this constructor explicitly
    -- when it lands rather than inherit one by accident.
    Serve Int

-- | The inputs only the indexing path needs.
data SyncOptions = SyncOptions
  { syNode :: SocketPath
  -- ^ Node-to-client socket of the local node to follow.
  , syNetwork :: NetworkId
  -- ^ Network of that node (its testnet magic).
  , syBatchSize :: Int
  -- ^ Commit to SQLite every this many written rows.
  , sySelectors :: [Selector]
  -- ^ Selectors to sieve outputs against, exactly as given. Empty means none
  -- were specified, which 'reconcileSelectors' reads as "use the database's
  -- own", falling back to the wildcard only when it has none either.
  , sySince :: ChainPoint
  -- ^ Chain point to start indexing from (@--since@); 'ChainPointAtGenesis' when
  -- omitted.
  , syUntil :: Maybe SlotNo
  -- ^ Slot to stop indexing at, inclusive (@--until@); 'Nothing' follows the
  -- chain indefinitely.
  , syRedeemers :: RedeemerCapture
  -- ^ Whether to also capture the redeemer that authorised each spend.
  }

-- | A validated invocation: the database every mode reads or writes, and what to
-- do with it.
data Invocation = Invocation
  { invDatabase :: FilePath
  , invCommand :: Command
  }

-- | The command line as parsed, before it is checked for coherence.
--
-- Kept flat and permissive on purpose: the node options stay 'Maybe' so that
-- @--serve@ and @--build-indexes@ still /accept/ a @--socket-path@ and
-- @--testnet-magic@ (existing scripts pass them) while no longer /requiring/
-- them. 'invocationOf' is where this becomes a 'Command', and where a sync
-- missing a node option is rejected.
data RawOptions = RawOptions
  { rawSocketPath :: Maybe SocketPath
  , rawNetworkId :: Maybe NetworkId
  , rawDatabasePath :: FilePath
  , rawBatchSize :: Int
  , rawSelectors :: [Selector]
  , rawSincePoint :: ChainPoint
  , rawUntilSlot :: Maybe SlotNo
  , rawWithRedeemers :: Bool
  , rawBuildIndexes :: Bool
  , rawServePort :: Maybe Int
  }

-- | Parse options and stream the chain, writing each header to SQLite and
-- printing each block number until the socket drops or the process is
-- interrupted.
sieve :: IO ()
sieve = do
  -- Line-buffer stdout so block numbers stream out promptly and are not lost
  -- when the process is killed (stdout is block-buffered by default when it is
  -- a pipe or file rather than a terminal).
  hSetBuffering stdout LineBuffering
  -- The RTS turns SIGINT into an async exception (so 'bracket'/'finally' run),
  -- but leaves SIGTERM as an immediate kill. SIGTERM is what @timeout@, @kill@,
  -- systemd and @docker stop@ send, so without this the final uncommitted
  -- SQLite batch would be lost on shutdown. Re-raise it as an interrupt on the
  -- main thread so the database is flushed and closed cleanly.
  mainThread <- myThreadId
  _ <- installHandler sigTERM (CatchOnce (throwTo mainThread UserInterrupt)) Nothing
  raw <- execParser optionsInfo
  Invocation{invDatabase = db, invCommand = cmd} <-
    either (die . ("cardano-sieve: " <>)) pure (invocationOf raw)
  case cmd of
    BuildIndexes -> installIndexes db
    Serve port -> runServer db port
    -- Indexing alone: catch-up runs in bulk mode (journaling off) and the
    -- session goes durable on reaching the tip. The trade is announced at
    -- startup and guarded by the dirty flag; see 'Durability'.
    Sync sync Nothing -> runSyncCommand UnsafeBulk db sync
    Sync sync (Just port) -> do
      -- Create the schema before starting either side. The server refuses to
      -- start against a missing database — a deliberate guard against a mistyped
      -- --database — but here the sync is what would create it, so on a fresh
      -- database the two race and the server always wins and dies. Doing it up
      -- front removes the race rather than weakening the guard; it is
      -- CREATE TABLE IF NOT EXISTS throughout, so an existing database is
      -- untouched.
      bracket (openDatabase Durable db (syBatchSize sync)) closeDatabase (const (pure ()))
      -- 'race_' rather than a bare fork so whichever finishes or dies first takes
      -- the other with it: a bounded sync reaching --until should end the
      -- process, and a crashed server should not leave a headless indexer behind.
      -- Serving alongside: the readers need WAL to coexist with the writer
      -- (journal-off would have every query fight the sync for the file), so
      -- this mode forgoes the bulk-speed trade.
      race_ (runSyncCommand Durable db sync) (runServer db port)
 where
  runSyncCommand
    durability
    db
    SyncOptions
      { syNode = node
      , syNetwork = network
      , syBatchSize = batch
      , sySelectors = selectors
      , sySince = since
      , syUntil = until_
      , syRedeemers = capture
      } =
      case until_ of
        Nothing -> fetch node network db batch durability capture selectors since
        Just u -> fetchBounded node network db batch durability capture selectors since u

-- | Check a parsed command line for coherence and resolve it into the mode it
-- names. The parser accepts each option in isolation; this is where the
-- combinations are judged.
--
-- @--serve@ alongside the node options means index and serve together;
-- @--serve@ without them means serve an already-synced database. That is why the
-- node options are optional in 'RawOptions' rather than required: which mode a
-- @--serve@ names depends on whether they are present.
invocationOf :: RawOptions -> Either String Invocation
invocationOf raw =
  Invocation (rawDatabasePath raw) <$> command
 where
  command
    | rawBuildIndexes raw =
        if rawServePort raw /= Nothing
          then
            Left "--build-indexes and --serve do nothing together: it exits as soon as the indexes are built"
          else Right BuildIndexes
    -- A --serve with no node to follow serves whatever is already on disk. The
    -- other node options are ignored rather than rejected: scripts pass them
    -- uniformly across invocations, and refusing would break that for no gain.
    | Just port <- rawServePort raw
    , Nothing <- rawSocketPath raw =
        Right (Serve port)
    | otherwise = Sync <$> syncOptions <*> pure (rawServePort raw)

  syncOptions = do
    node <- required "--socket-path" (rawSocketPath raw)
    network <- required "--testnet-magic" (rawNetworkId raw)
    pure
      SyncOptions
        { syNode = node
        , syNetwork = network
        , syBatchSize = rawBatchSize raw
        , -- Passed through EMPTY when no --select is given, rather than
          -- defaulted to the wildcard here. Empty has to survive as far as
          -- 'reconcileSelectors', because there it means "adopt whatever this
          -- database was built with" — defaulting first turns a plain restart
          -- into a selector conflict against every non-wildcard database.
          sySelectors = rawSelectors raw
        , sySince = rawSincePoint raw
        , syUntil = rawUntilSlot raw
        , syRedeemers = if rawWithRedeemers raw then CaptureRedeemers else SkipRedeemers
        }

  required flag =
    maybe
      (Left (flag <> " is required to sync (omit it, with --serve, to serve an existing database)"))
      Right

optionsInfo :: ParserInfo RawOptions
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Follow a local node's chain, sieve each block's outputs against the \
          \configured selectors, and index the matches into SQLite — optionally \
          \serving the read query API from the same process"
        <> header "cardano-sieve - pattern-filtered chain index"
    )

optionsParser :: Parser RawOptions
optionsParser =
  RawOptions
    <$> pSocketPath
    <*> pNetworkId
    <*> pDatabasePath
    <*> pBatchSize
    <*> pSelectors
    <*> pSince
    <*> pUntil
    <*> pWithRedeemers
    <*> pBuildIndexes
    <*> pServe
 where
  -- Optional, not required: only a sync needs a node. 'invocationOf' demands it
  -- for the modes that actually connect, and its presence alongside --serve is
  -- what distinguishes "index and serve" from "serve an existing database".
  pSocketPath :: Parser (Maybe SocketPath)
  pSocketPath =
    optional $
      File
        <$> strOption
          ( long "socket-path"
              <> metavar "FILEPATH"
              <> help "Path to the local node's node-to-client socket (required to sync)"
          )

  pNetworkId :: Parser (Maybe NetworkId)
  pNetworkId =
    optional $
      Testnet . NetworkMagic
        <$> option
          auto
          ( long "testnet-magic"
              <> metavar "MAGIC"
              <> help "Testnet network magic, e.g. 42 (required to sync)"
          )

  pDatabasePath :: Parser FilePath
  pDatabasePath =
    strOption
      ( long "database"
          <> metavar "FILEPATH"
          <> help "SQLite database file to write block headers into"
      )

  pBatchSize :: Parser Int
  pBatchSize =
    option
      auto
      ( long "batch-size"
          <> metavar "N"
          -- 50000 is near the bottom of the commit-overhead vs WAL-bloat
          -- U-curve for bulk sync (see bench/batch-sweep.sh): too small means
          -- frequent COMMITs and fsync stalls; too big means the WAL can't
          -- checkpoint mid-transaction, grows huge, and every read slows down.
          -- (A fixed value is a bulk-sync compromise; tip-following ideally
          -- wants smaller/adaptive batches for query freshness.)
          <> value 50000
          <> showDefault
          <> help "Commit to SQLite every N written rows (outputs + spends)"
      )

  pSelectors :: Parser [Selector]
  pSelectors =
    many $
      option
        (eitherReader (first show . selectorFromText . T.pack))
        ( long "select"
            <> metavar "SELECTOR"
            <> help
              "Selector to index, e.g. an address or 'policyid.*' (repeatable; omit to index every output)"
        )

  pSince :: Parser ChainPoint
  pSince =
    option
      (eitherReader readChainPoint)
      ( long "since"
          <> metavar "SLOT.HEADERHASH"
          <> value ChainPointAtGenesis
          <> showDefaultWith (const "origin")
          <> help "Point to start indexing from: 'origin' or SLOT.HEADERHASH"
      )

  pUntil :: Parser (Maybe SlotNo)
  pUntil =
    optional $
      option
        (SlotNo <$> auto)
        ( long "until"
            <> metavar "SLOT"
            <> help "Stop indexing after this slot, inclusive (default: follow the chain)"
        )

  pWithRedeemers :: Parser Bool
  pWithRedeemers =
    switch
      ( long "with-redeemers"
          <> help
            "Also store the redeemer that authorised each spend (opt-in: redeemers are \
            \the heavy bytes on the spend path)"
      )

  pBuildIndexes :: Parser Bool
  pBuildIndexes =
    switch
      ( long "build-indexes"
          <> help "Install the deferred query indexes on --database and exit (run once after the initial sync)"
      )

  pServe :: Parser (Maybe Int)
  pServe =
    optional $
      option
        auto
        ( long "serve"
            <> metavar "PORT"
            <> help
              "Serve the read query API on this port. With --socket-path it indexes and \
              \serves together; without, it serves an existing --database and does not sync"
        )

-- | Parse a @--since@ argument: @origin@, or @SLOT.HEADERHASH@ — a decimal slot
-- and a base16 block-header hash, as kupo and cardano-cli render chain points.
readChainPoint :: String -> Either String ChainPoint
readChainPoint "origin" = Right ChainPointAtGenesis
readChainPoint s =
  case break (== '.') s of
    (slotStr, '.' : hashStr)
      | Just slot <- readMaybe slotStr ->
          first show $
            ChainPoint (SlotNo slot)
              <$> deserialiseFromRawBytesHex @(Hash BlockHeader) (encodeUtf8 (T.pack hashStr))
    _ -> Left "expected 'origin' or SLOT.HEADERHASH"

{-

Overall architecture (minus supporting rollbacks)

Phase 1
cardano-api - chain-sync or ogmios or cardano-rpc to get blocks
  - This needs a wrapper around it to  switch between the different sources of blocks
  - start with local node chain-sync - look at Cardano.Api.LedgerSTate to get an idea of how to construct the ChainSyncClientPipelined

decode blocks
  - Full block decode at first

stuff into database
  - Just drop block headers (or whatever) into the database for now

Phase 2 - Parsers/Sieves

Figure out how we are going to apply sieves for the different address related things

Phase 3 - Rollbacks and DAtabase schema

These two are related because you a schema that allows you to cascade deletes easily in the event of a rollback

Properties we want

If our source of blocks disconnects we are alerted and we automatically try to reconnect
Rollforward and rollback = id

Questions
- is memory consumption a problem? How do we easily measure this?

  -}
