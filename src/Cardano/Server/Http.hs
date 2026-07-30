{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Read query API (servant + warp) over the synced SQLite database.
--
-- First vertical slice: unspent UTxOs by address —
-- @GET \/matches\/{address}?unspent@ — returning JSON rows straight from the
-- @unspent@ table (which the query indexes back). The response shape and the
-- other match dimensions (credentials, policy/asset, slot range, wildcard) are
-- still to be aligned with kupo; this exists to get one endpoint end-to-end.
--
-- The address is taken as base16 for now (kupo's bech32/base58 forms, and the
-- pattern-in-path grammar, come next). A fresh read connection is opened per
-- request; a connection pool is a later refinement.
module Cardano.Server.Http
  ( runServer
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null), eitherDecodeStrict, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple (Only (Only), query, withConnection)
import Network.Wai.Handler.Warp qualified as Warp

import Servant (Capture, Get, Handler, JSON, QueryFlag, Server, serve, (:>))

-- | The query API. One endpoint for now.
type API =
  "matches"
    :> Capture "address" Text
    :> QueryFlag "unspent"
    :> Get '[JSON] [Value]

-- | Serve the query API on @port@, reading from the SQLite database at @dbPath@.
runServer :: FilePath -> Int -> IO ()
runServer dbPath port =
  Warp.run port (serve (Proxy :: Proxy API) (server dbPath))

server :: FilePath -> Server API
server = matchesByAddress

-- | Unspent UTxOs at one address, newest first. Bad hex yields an empty list
-- for now (proper 400s come with the real request parsing).
matchesByAddress :: FilePath -> Text -> Bool -> Handler [Value]
matchesByAddress dbPath addrHex _unspent =
  case Base16.decode (encodeUtf8 addrHex) of
    Left _ -> pure []
    Right addr ->
      liftIO $
        withConnection dbPath $ \conn -> do
          rows <-
            query
              conn
              "SELECT output_reference, address, value, created_slot \
              \FROM unspent WHERE address = ? ORDER BY created_slot DESC LIMIT 100"
              (Only addr)
          pure (map rowToJson rows)

-- | Render one @unspent@ row as JSON. @value@ is stored as JSON already, so it
-- is re-parsed and nested rather than escaped as a string.
rowToJson :: (ByteString, ByteString, ByteString, Int64) -> Value
rowToJson (oref, addr, val, slot) =
  object
    [ "output_reference" .= hexText oref
    , "address" .= hexText addr
    , "value" .= either (const Null) id (eitherDecodeStrict val)
    , "created_slot" .= slot
    ]
 where
  hexText :: ByteString -> Text
  hexText = decodeUtf8 . Base16.encode
