{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/scripts@ — the script behind a hash.
module Cardano.Sieve.Server.Api.Scripts
  ( ScriptsAPI
  , scriptsServer
  )
where

import Cardano.Sieve.Server.Api.Common (hexText, lookupByHash, scriptLanguage)

import Data.Aeson (Value (Null), object, (.=))
import Data.ByteString qualified as BS
import Data.Text (Text)

import Servant (Capture, Get, JSON, Server, (:>))

-- | @GET \/scripts\/{hash}@ — the script behind a hash, or @null@ when unknown
-- (matching @\/datums@).
--
-- Shape: @{"script": "<hex>", "language": "native"|"plutus:v1"|…}@. The stored
-- blob is the exact bytes the script hash is computed over, which carry the
-- language discriminator as their leading byte, so the byte is split back off
-- here: @language@ names it and @script@ is the raw script without it.
type ScriptsAPI = "scripts" :> Capture "script-hash" Text :> Get '[JSON] Value

scriptsServer :: FilePath -> Server ScriptsAPI
scriptsServer dbPath h =
  lookupByHash dbPath h "SELECT script FROM scripts WHERE script_hash = ?" $ \stored ->
    case BS.uncons stored of
      Nothing -> Null
      Just (tag, raw) ->
        object ["script" .= hexText raw, "language" .= scriptLanguage tag]
