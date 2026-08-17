{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/datums@ and @\/scripts@ — preimage lookups by hash.
module Cardano.Server.Api.Preimages
  ( PreimageAPI
  , preimageServer
  )
where

import Cardano.Server.Api.Common (hexText, scriptLanguage, withReadConnection)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null), object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Only (Only), Query, query)

import Servant (Capture, Get, Handler, JSON, Server, (:<|>) ((:<|>)), (:>))

-- | Preimage lookups by hash: the bodies behind the hashes a match reports.
--
-- Both return @null@ rather than a 404 for an unknown hash, as kupo does — a
-- referenced datum whose body has not been seen on chain is a normal state, not an
-- error.
type PreimageAPI =
  "datums" :> Capture "datum-hash" Text :> Get '[JSON] Value
    :<|> "scripts" :> Capture "script-hash" Text :> Get '[JSON] Value

-- | Both routes.
preimageServer :: FilePath -> Server PreimageAPI
preimageServer dbPath = datumByHash dbPath :<|> scriptByHash dbPath

-- | @GET \/datums\/{hash}@ — the datum body behind a hash, or @null@.
--
-- Shape is kupo's: @{"datum": "<hex>"}@.
datumByHash :: FilePath -> Text -> Handler Value
datumByHash dbPath h =
  preimage dbPath h "SELECT datum FROM binary_data WHERE datum_hash = ?" $ \body ->
    object ["datum" .= hexText body]

-- | @GET \/scripts\/{hash}@ — the script body behind a hash, or @null@.
--
-- Shape is kupo's: @{"script": "<hex>", "language": "native"|"plutus:v1"|…}@. The
-- stored blob is the hash preimage, which carries the language discriminator as
-- its leading byte, so the byte is split back off here: @language@ names it and
-- @script@ is the raw script without it. kupo documents the same split — "raw
-- scripts aren't exact pre-image of their hash digest".
scriptByHash :: FilePath -> Text -> Handler Value
scriptByHash dbPath h =
  preimage dbPath h "SELECT script FROM scripts WHERE script_hash = ?" $ \body ->
    case BS.uncons body of
      Nothing -> Null
      Just (tag, raw) ->
        object ["script" .= hexText raw, "language" .= scriptLanguage tag]

-- | Look one preimage up by its hex hash. A malformed hash and an absent row are
-- both @null@: neither is a client error worth a 400, and kupo answers @null@ too.
preimage :: FilePath -> Text -> Query -> (ByteString -> Value) -> Handler Value
preimage dbPath h sql render =
  case Base16.decode (encodeUtf8 h) of
    Left _ -> pure Null
    Right raw -> liftIO $ withReadConnection dbPath $ \conn -> do
      rows <- query conn sql (Only raw)
      pure $ case rows of
        Only body : _ -> render body
        [] -> Null
