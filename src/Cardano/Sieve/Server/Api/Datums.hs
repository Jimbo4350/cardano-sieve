{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/datums@ — the datum behind a hash.
module Cardano.Sieve.Server.Api.Datums
  ( DatumsAPI
  , datumsServer
  )
where

import Cardano.Sieve.Server.Api.Common (hexText, lookupByHash)

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Servant (Capture, Get, JSON, Server, (:>))

-- | @GET \/datums\/{hash}@ — the datum behind a hash, or @null@.
--
-- @null@ rather than a 404 for an unknown hash: an output can reference a datum
-- whose bytes have not been seen on chain, so an absent datum is a normal state,
-- not an error.
--
-- Shape: @{"datum": "<hex>"}@.
type DatumsAPI = "datums" :> Capture "datum-hash" Text :> Get '[JSON] Value

datumsServer :: FilePath -> Server DatumsAPI
datumsServer dbPath h =
  lookupByHash dbPath h "SELECT datum FROM binary_data WHERE datum_hash = ?" $ \datum ->
    object ["datum" .= hexText datum]
