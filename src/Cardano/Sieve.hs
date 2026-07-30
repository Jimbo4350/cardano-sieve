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
import Cardano.Sieve.Node.Insert (installIndexes)
import Cardano.Sieve.Selector
  ( BootstrapFilter (IncludeBootstrap)
  , Selector (SelectAll)
  , selectorFromText
  )
import Cardano.Slotting.Slot (SlotNo (SlotNo))

import Control.Applicative (many, optional)
import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
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
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigTERM)
import Text.Read (readMaybe)

-- | Command-line options for the @cardano-sieve@ executable.
data Options = Options
  { socketPath :: SocketPath
  -- ^ Node-to-client socket of the local node to follow.
  , networkId :: NetworkId
  -- ^ Network of that node (its testnet magic).
  , databasePath :: FilePath
  -- ^ SQLite file to write block headers into.
  , batchSize :: Int
  -- ^ Commit to SQLite every this many headers.
  , selectors :: [Selector]
  -- ^ Selectors to sieve outputs against (from repeatable @--select@); an empty
  -- list means match every output.
  , sincePoint :: ChainPoint
  -- ^ Chain point to start indexing from (@--since@); 'ChainPointAtGenesis' when
  -- omitted.
  , untilSlot :: Maybe SlotNo
  -- ^ Slot to stop indexing at, inclusive (@--until@); 'Nothing' follows the
  -- chain indefinitely.
  , buildIndexes :: Bool
  -- ^ @--build-indexes@: instead of syncing, install the deferred query indexes
  -- on @--database@ and exit. Run once after the initial sync.
  , servePort :: Maybe Int
  -- ^ @--serve PORT@: instead of syncing, serve the read query API on this port.
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
  opts <- execParser optionsInfo
  case servePort opts of
    Just p -> runServer (databasePath opts) p
    Nothing
      | buildIndexes opts -> installIndexes (databasePath opts)
      | otherwise -> case untilSlot opts of
          Nothing ->
            fetch
              (socketPath opts)
              (networkId opts)
              (databasePath opts)
              (batchSize opts)
              (configuredSelectors opts)
              (sincePoint opts)
          Just u ->
            fetchBounded
              (socketPath opts)
              (networkId opts)
              (databasePath opts)
              (batchSize opts)
              (configuredSelectors opts)
              (sincePoint opts)
              u
 where
  -- Default to matching every output when no --select is given, so the full
  -- decode → sieve → write path is still exercised out of the box.
  configuredSelectors o = case selectors o of
    [] -> [SelectAll IncludeBootstrap]
    xs -> xs

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Follow a local node's chain and print each block number"
        <> header "cardano-sieve - pattern-filtered chain index"
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> pSocketPath
    <*> pNetworkId
    <*> pDatabasePath
    <*> pBatchSize
    <*> pSelectors
    <*> pSince
    <*> pUntil
    <*> pBuildIndexes
    <*> pServe
 where
  pSocketPath :: Parser SocketPath
  pSocketPath =
    File
      <$> strOption
        ( long "socket-path"
            <> metavar "FILEPATH"
            <> help "Path to the local node's node-to-client socket"
        )

  pNetworkId :: Parser NetworkId
  pNetworkId =
    Testnet . NetworkMagic
      <$> option
        auto
        ( long "testnet-magic"
            <> metavar "MAGIC"
            <> help "Testnet network magic (e.g. 42)"
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
            <> help "Serve the read query API on this port (reads --database; does not sync)"
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
