{-# LANGUAGE ImportQualifiedPost #-}

-- | Executable entry point. Parses command-line options, follows a local node's
-- chain, sieves each block's outputs against the configured selectors, and
-- writes the matches into SQLite, printing each block number and its match
-- count as it is rolled forward.
--
-- Connect over node-to-client ChainSync, decode each block, run the sieve, and
-- persist matched outputs (ADR-020 and the architecture notes at the bottom of
-- this module). The selector surface syntax is not wired to the CLI yet, so
-- every output is matched for now.
module Cardano.Sieve
  ( sieve
  )
where

import Cardano.Api
  ( File (File)
  , NetworkId (Testnet)
  , NetworkMagic (NetworkMagic)
  , SocketPath
  )

import Cardano.Sieve.Node.Fetch (fetch)
import Cardano.Sieve.Selector (BootstrapFilter (IncludeBootstrap), Selector (SelectAll))

import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Options.Applicative
  ( Parser
  , ParserInfo
  , auto
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
  , strOption
  , value
  , (<**>)
  )
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigTERM)

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
  fetch (socketPath opts) (networkId opts) (databasePath opts) (batchSize opts) selectors
 where
  -- No surface syntax for selectors yet (see
  -- notes/Kupo-rewrite-pattern-and-storage-design.md), so default to matching
  -- every output. This exercises the full decode → sieve → write path.
  selectors = [SelectAll IncludeBootstrap]

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
          <> value 1000
          <> showDefault
          <> help "Commit to SQLite every N block headers"
      )

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
