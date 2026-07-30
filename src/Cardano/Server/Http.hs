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

import Cardano.Api (AsType (AsAddressAny), deserialiseFromRawBytes, serialiseAddress)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), eitherDecodeStrict, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64)
import Database.SQLite.Simple (Only (Only), query, withConnection)
import GHC.Clock (getMonotonicTime)
import Network.Wai (Middleware, rawPathInfo, rawQueryString, requestMethod)
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
runServer dbPath port = do
  putStrLn
    ("cardano-sieve query API: http://127.0.0.1:" <> show port <> "  (database " <> dbPath <> ")")
  Warp.run port (logRequests (serve (Proxy :: Proxy API) (server dbPath)))

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

server :: FilePath -> Server API
server = matchesByAddress

-- | Unspent UTxOs at one address, newest first, in kupo's response shape. Bad
-- hex yields an empty list (proper 400s come with the real request parsing).
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
              "SELECT u.output_reference, u.address, u.value, u.datum_hash, \
              \u.reference_script_hash, u.created_slot, b.header_hash \
              \FROM unspent u LEFT JOIN blocks b ON b.slot_no = u.created_slot \
              \WHERE u.address = ? ORDER BY u.created_slot DESC LIMIT 100"
              (Only addr)
          pure (map rowToJson rows)

-- | One @unspent@ row as JSON, mirroring kupo's match shape. Two kupo fields are
-- omitted because sieve does not track them yet: @transaction_index@ (position
-- of the tx within its block) and @datum_type@ (inline vs hash).
rowToJson
  :: (ByteString, ByteString, ByteString, Maybe ByteString, Maybe ByteString, Int64, Maybe ByteString)
  -> Value
rowToJson (oref, addr, val, mDatum, mScript, slot, mHeader) =
  object
    [ "transaction_id" .= hexText (BS.take 32 oref)
    , "output_index" .= outputIndex oref
    , "address" .= addressText addr
    , "value" .= kupoValue val
    , "datum_hash" .= (hexText <$> mDatum)
    , "script_hash" .= (hexText <$> mScript)
    , "created_at" .= object ["slot_no" .= slot, "header_hash" .= (hexText <$> mHeader)]
    , "spent_at" .= Null
    ]

-- | Base16 of raw bytes, as text.
hexText :: ByteString -> Text
hexText = decodeUtf8 . Base16.encode

-- | The output reference is 32 tx-id bytes then a big-endian Word64 output
-- index; recover the index.
outputIndex :: ByteString -> Word64
outputIndex = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 . BS.drop 32

-- | Render the raw address bytes as kupo does (bech32 for Shelley, base58 for
-- Byron); fall back to hex if it does not decode.
addressText :: ByteString -> Text
addressText raw = case deserialiseFromRawBytes AsAddressAny raw of
  Right a -> serialiseAddress a
  Left _ -> hexText raw

-- | Reshape sieve's stored value JSON (@{policy:{name:qty}, lovelace:n}@) into
-- kupo's @{coins, assets:{"policy.name":qty}}@, quantities rendered as strings.
kupoValue :: ByteString -> Value
kupoValue raw = case eitherDecodeStrict raw of
  Right (Object o) ->
    object
      [ "coins" .= maybe "0" numToStr (KM.lookup "lovelace" o)
      , "assets" .= Object (KM.foldrWithKey flatten KM.empty o)
      ]
  _ -> object ["coins" .= ("0" :: Text), "assets" .= object []]
 where
  flatten k v acc
    | k == "lovelace" = acc
    | otherwise = case v of
        Object names ->
          KM.foldrWithKey
            ( \name qty -> KM.insert (Key.fromText (Key.toText k <> "." <> Key.toText name)) (String (numToStr qty))
            )
            acc
            names
        _ -> acc

-- | An integer JSON number (or numeric string) as a decimal string.
numToStr :: Value -> Text
numToStr (Number s) = pack (show (floor s :: Integer))
numToStr (String t) = t
numToStr _ = "0"
