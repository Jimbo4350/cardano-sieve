{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Read query API (servant + warp) over the synced SQLite database.
--
-- The endpoints:
--
--   * @GET \/matches\/{pattern}@ — every dimension the indexer can match on
--   * @GET \/datums\/{hash}@ — the datum behind a hash, @{datum}@
--   * @GET \/scripts\/{hash}@ — the script behind a hash, @{script, language}@
--   * @GET \/checkpoints@ — a sample of stored chain points, newest first
--   * @GET \/checkpoints\/{slot-no}@ — the point at (or, by default, at-or-before)
--     a slot; @?strict@ demands the exact slot
--   * @GET \/patterns@ \/ @GET \/patterns\/{pattern}@ — the configured selectors,
--     all of them or those including a given pattern
--   * @PUT@\/@DELETE@ on @\/patterns@ — 501: reconfiguring a live indexer is a
--     deliberate non-feature; restart it with different @--select@s instead
--   * @DELETE \/matches\/{pattern}@ — prune everything a pattern matched,
--     refused while a configured selector still covers it
--   * @GET \/health@ — JSON health; @GET \/metrics@ — the same
--     facts in Prometheus exposition
--   * @GET \/metadata\/{slot-no}@ — a block's transaction metadata, fetched
--     from the node on demand, never stored; needs a node, so
--     serve-only mode refuses it
--
-- Each endpoint lives in its own @Cardano.Sieve.Server.Api.*@ module; this module
-- assembles them and runs the server.
module Cardano.Sieve.Server.Run
  ( runServer
  )
where

import Cardano.Api (NetworkId, SocketPath)

import Cardano.Sieve.Database.DirtyFlag (isDirty)
import Cardano.Sieve.Server.Api.Checkpoints (CheckpointsAPI, checkpointsServer)
import Cardano.Sieve.Server.Api.Common (withReadConnection)
import Cardano.Sieve.Server.Api.Datums (DatumsAPI, datumsServer)
import Cardano.Sieve.Server.Api.Health (HealthAPI, healthServer)
import Cardano.Sieve.Server.Api.Matches (MatchesAPI, matchesServer)
import Cardano.Sieve.Server.Api.Metadata (MetadataAPI, metadataServer)
import Cardano.Sieve.Server.Api.Patterns (PatternsAPI, patternsServer)
import Cardano.Sieve.Server.Api.Scripts (ScriptsAPI, scriptsServer)

import Control.Exception (SomeAsyncException (..), SomeException, fromException, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Database.SQLite.Simple (Connection, Only (Only), query_)
import GHC.Clock (getMonotonicTime)
import Network.Wai (Middleware, rawPathInfo, rawQueryString, requestMethod)
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (die)
import System.Posix.Files (fileExist)

import Servant (Server, serve, (:<|>) ((:<|>)))

-- | The query API.
type API =
  MatchesAPI
    :<|> CheckpointsAPI
    :<|> PatternsAPI
    :<|> HealthAPI
    :<|> DatumsAPI
    :<|> ScriptsAPI
    :<|> MetadataAPI

-- | Serve the query API on @port@, reading from the SQLite database at @dbPath@.
-- Refuses to start unless the database is present and readable ('describeDatabase').
runServer :: Maybe (SocketPath, NetworkId) -> FilePath -> Int -> IO ()
runServer node dbPath port = do
  summary <- describeDatabase dbPath
  putStrLn
    ( "cardano-sieve query API: http://127.0.0.1:"
        <> show port
        <> "  (database "
        <> dbPath
        <> " — "
        <> summary
        <> ")"
    )
  Warp.run port (logRequests (serve (Proxy :: Proxy API) (server node dbPath)))

-- | Check the database before serving from it, and describe what is in it.
--
-- Worth doing loudly because the failure is otherwise silent: 'withConnection' is
-- opened per request and SQLite /creates/ a missing file on open, so a deleted or
-- mistyped @--database@ path used to start a server that logged nothing (no
-- requests yet), then answered every query with @[]@. Dying here, and printing the
-- row count and tip slot on success, makes "is this pointed at real data?"
-- answerable from the startup line alone.
describeDatabase :: FilePath -> IO String
describeDatabase dbPath = do
  exists <- fileExist dbPath
  unless exists $
    die (dbPath <> ": no such database — sync one first, or check --database")
  probed <-
    try (withReadConnection dbPath probe) :: IO (Either SomeException (Int, Maybe Int64, Bool))
  case probed of
    -- 'SomeException' also catches the 'AsyncCancelled' that 'race_' delivers
    -- when the sync thread dies first. That is not a database problem —
    -- blaming the file for it buried the real error once — so cancellation
    -- (and any other async exception) is rethrown, not reported.
    Left err
      | Just (SomeAsyncException _) <- fromException err -> throwIO err
      | otherwise ->
          die (mconcat [dbPath, ": ", show err])
    Right (rows, tip, indexed) -> do
      when (rows == 0) $
        putStrLn "WARNING: 0 unspent rows — every query will return []. Is the sync finished?"
      unless indexed $
        putStrLn "WARNING: query indexes absent — queries will be slow. Run with --build-indexes."
      pure
        ( show rows
            <> " unspent rows, tip slot "
            <> maybe "none" show tip
            <> if indexed then ", indexed" else ", NOT indexed"
        )
 where
  headOr :: a -> [Only a] -> a
  headOr d rs = case rs of Only x : _ -> x; [] -> d

  probe :: Connection -> IO (Int, Maybe Int64, Bool)
  probe conn = do
    -- Refuse a file left dirty by a crashed bulk sync before reporting anything
    -- from it — with journaling off there is no telling what state it is in, and
    -- serving confidently-wrong rows is worse than not starting.
    dirty <- isDirty conn
    when dirty $
      die
        ( dbPath
            <> ": left dirty by an interrupted bulk sync — its contents cannot \
               \be trusted. Delete it and sync again."
        )
    rows <- query_ conn "SELECT count(*) FROM unspent"
    tips <- query_ conn "SELECT max(created_slot) FROM unspent"
    -- One representative deferred index; they are all installed together.
    idxs <-
      query_
        conn
        "SELECT count(*) FROM sqlite_master WHERE type='index' AND name='unspentByAddress'"
    pure (headOr 0 rows, headOr Nothing tips, headOr (0 :: Int) idxs > 0)

-- | Minimal request log — "METHOD path?query  <ms>" per request — so it is
-- obvious the server is alive and requests are landing. The per-line cost is
-- negligible next to the SQL query and the HTTP round-trip.
logRequests :: Middleware
logRequests app req respond = do
  t0 <- getMonotonicTime
  app req $ \res -> do
    sent <- respond res
    t1 <- getMonotonicTime
    putStrLn $
      B8.unpack (requestMethod req)
        <> " "
        <> B8.unpack (rawPathInfo req)
        <> B8.unpack (rawQueryString req)
        <> "  "
        <> show (round ((t1 - t0) * 1000) :: Int)
        <> "ms"
    pure sent

server :: Maybe (SocketPath, NetworkId) -> FilePath -> Server API
server node dbPath =
  matchesServer dbPath
    :<|> checkpointsServer dbPath
    :<|> patternsServer dbPath
    :<|> healthServer node dbPath
    :<|> datumsServer dbPath
    :<|> scriptsServer dbPath
    :<|> metadataServer node dbPath
