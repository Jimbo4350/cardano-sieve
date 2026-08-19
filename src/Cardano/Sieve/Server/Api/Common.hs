{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers shared by the server and its endpoint modules.
module Cardano.Sieve.Server.Api.Common
  ( withReadConnection
  , badRequest
  , hexText
  , lookupByHash
  , scriptLanguage
  )
where

import Cardano.Sieve.Node.Insert (busyTimeoutMs)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null, String))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Database.SQLite.Simple (Connection, Only (Only), Query, query, query_, withConnection)

import Servant (Handler, ServerError (errBody), err400, throwError)

-- | Open a read connection, matching the writer's lock-wait policy.
--
-- A plain 'withConnection' inherits SQLite's default @busy_timeout@ of 0 ms —
-- return @SQLITE_BUSY@ rather than wait. That is invisible while the server has
-- the database to itself, and breaks the moment an indexer shares the process
-- (@--serve@ alongside @--socket-path@): WAL keeps ordinary reads clear of the
-- writer, but the brief exclusive moments still collide and, with no timeout, the
-- reader errors instead of waiting a few milliseconds. Observed directly — a
-- fresh sync-and-serve failed with @ErrorBusy … database is locked@ on the
-- startup probe.
--
-- A connection is still opened per request; pooling is a separate refinement.
withReadConnection :: FilePath -> (Connection -> IO a) -> IO a
withReadConnection dbPath act =
  withConnection dbPath $ \conn -> do
    () <$ (query_ conn ("PRAGMA busy_timeout=" <> busyTimeoutMs) :: IO [Only Int])
    act conn

-- | Reject a request with the reason in the body.
badRequest :: Text -> Handler a
badRequest msg = throwError err400{errBody = LBS.fromStrict (encodeUtf8 msg)}

-- | Base16 of raw bytes, as text.
hexText :: ByteString -> Text
hexText = decodeUtf8 . Base16.encode

-- | Look one datum or script up by its hex hash: the shared shape of
-- @\/datums\/{hash}@ and @\/scripts\/{hash}@. A malformed hash and an absent row
-- are both @null@: neither is a client error worth a 400.
lookupByHash :: FilePath -> Text -> Query -> (ByteString -> Value) -> Handler Value
lookupByHash dbPath h sql render =
  case Base16.decode (encodeUtf8 h) of
    Left _ -> pure Null
    Right raw -> liftIO $ withReadConnection dbPath $ \conn -> do
      rows <- query conn sql (Only raw)
      pure $ case rows of
        Only stored : _ -> render stored
        [] -> Null

-- | The language a stored script's discriminator byte names.
scriptLanguage :: Word8 -> Value
scriptLanguage = \case
  0 -> "native"
  1 -> "plutus:v1"
  2 -> "plutus:v2"
  3 -> "plutus:v3"
  n -> String ("unknown:" <> pack (show n))
