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
--   * @GET \/datums\/{hash}@ — a datum preimage, @{datum}@
--   * @GET \/scripts\/{hash}@ — a script preimage, @{script, language}@
--   * @GET \/checkpoints@ — a sample of stored chain points, newest first
--   * @GET \/checkpoints\/{slot-no}@ — the point at (or, by default, at-or-before)
--     a slot; @?strict@ demands the exact slot
--   * @GET \/patterns@ \/ @GET \/patterns\/{pattern}@ — the configured selectors,
--     all of them or those including a given pattern
--   * @PUT@\/@DELETE@ on @\/patterns@ — 501: reconfiguring a live indexer is a
--     deliberate non-feature; restart it with different @--select@s instead
--   * @DELETE \/matches\/{pattern}@ — prune everything a pattern matched,
--     refused while a configured selector still covers it
--   * @GET \/health@ — JSON health, kupo-shaped; @GET \/metrics@ — the same
--     facts in Prometheus exposition
--   * @GET \/metadata\/{slot-no}@ — a block's transaction metadata, fetched
--     from the node on demand (kupo stores none either); needs a node, so
--     serve-only mode refuses it
--
-- Each endpoint lives in its own @Cardano.Server.Api.*@ module; this module
-- assembles them and runs the server.
module Cardano.Server.Run
  ( runServer
  )
where

import Cardano.Api (NetworkId, SocketPath)

import Cardano.Server.Api.Checkpoints (CheckpointsAPI, checkpointsServer)
import Cardano.Server.Api.Common (hexText, withReadConnection)
import Cardano.Server.Api.Health (HealthAPI, healthServer)
import Cardano.Server.Api.Matches (MatchesAPI, matchesServer)
import Cardano.Server.Api.Metadata (MetadataAPI, metadataServer)
import Cardano.Server.Api.Patterns (PatternsAPI, patternsServer)
import Cardano.Server.Api.Preimages (PreimageAPI, preimageServer)
import Cardano.Sieve.Database (isDirty)

import Control.Exception (SomeAsyncException (..), SomeException, fromException, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection, Only (Only), query_)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types.Status (status304)
import Network.Wai
  ( Middleware
  , mapResponseHeaders
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , requestMethod
  , responseLBS
  )
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (die)
import System.Posix.Files (fileExist)

import Servant (Server, serve, (:<|>) ((:<|>)))

-- | The query API.
type API =
  MatchesAPI :<|> CheckpointsAPI :<|> PatternsAPI :<|> HealthAPI :<|> PreimageAPI :<|> MetadataAPI

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
  Warp.run port (logRequests (cacheHeaders dbPath (serve (Proxy :: Proxy API) (server node dbPath))))

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

-- | Conditional-request support, kupo's contract exactly.
--
-- Every response carries @X-Most-Recent-Checkpoint@ (the newest indexed slot,
-- @0@ when the database is empty) and, when a checkpoint exists, @ETag@ — the
-- tip block's header hash as bare hex, no quotes. A request whose
-- @If-None-Match@ equals the current tag short-circuits to an empty @304@
-- before any handler runs: the chain has not moved since the client last
-- looked, so neither has any answer this server could give. That is what makes
-- polling cheap — kupo's spec documents the same @304@ on its read endpoints.
--
-- The tag is deliberately the raw hex kupo compares with plain equality, not an
-- RFC-quoted validator: kupo clients send back exactly what @ETag@ carried, and
-- matching kupo means matching that byte-for-byte.
--
-- One point lookup per request (the newest checkpoint, off the primary key).
-- kupo answers this from an in-memory health record instead; a cached tip is a
-- later refinement alongside the connection pool.
cacheHeaders :: FilePath -> Middleware
cacheHeaders dbPath app req respond = do
  tip <- withReadConnection dbPath $ \conn ->
    query_ conn "SELECT slot_no, header_hash FROM checkpoints ORDER BY slot_no DESC LIMIT 1"
      :: IO [(Int64, ByteString)]
  case tip of
    [] ->
      app req (respond . mapResponseHeaders (("X-Most-Recent-Checkpoint", "0") :))
    (slot, hash) : _ -> do
      let etag = encodeUtf8 (hexText hash)
          headers =
            [ ("X-Most-Recent-Checkpoint", B8.pack (show slot))
            , ("ETag", etag)
            ]
      if lookup "if-none-match" (requestHeaders req) == Just etag
        then respond (responseLBS status304 headers "")
        else app req (respond . mapResponseHeaders (headers <>))

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
    :<|> preimageServer dbPath
    :<|> metadataServer node dbPath
