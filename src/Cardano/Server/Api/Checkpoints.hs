{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/checkpoints@ — chain points the indexer has recorded.
module Cardano.Server.Api.Checkpoints
  ( CheckpointsAPI
  , checkpointsServer
  )
where

import Cardano.Server.Api.Common (hexText, withReadConnection)
import Cardano.Sieve.Node.Insert (sampleCheckpoints)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null), object, (.=))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Database.SQLite.Simple (Only (Only), query)

import Servant (Capture, Get, Handler, JSON, QueryFlag, Server, (:<|>) ((:<|>)), (:>))

-- | Chain points the indexer has recorded, kupo-shaped.
--
-- The list endpoint is a /sample/ (the exponential ladder shared with resume,
-- 'sampleCheckpoints') — one checkpoint per applied block exists underneath,
-- which nobody wants in one response. The by-slot endpoint answers with the
-- point at-or-before the slot unless @?strict@ demands an exact hit; an absent
-- point is @null@, not a 404, matching kupo and \/datums.
type CheckpointsAPI =
  "checkpoints" :> Get '[JSON] [Value]
    :<|> "checkpoints" :> Capture "slot-no" Int64 :> QueryFlag "strict" :> Get '[JSON] Value

-- | Both routes.
checkpointsServer :: FilePath -> Server CheckpointsAPI
checkpointsServer dbPath = checkpointsSample dbPath :<|> checkpointBySlot dbPath

-- | @GET \/checkpoints@ — the stored chain points, sampled newest-first.
checkpointsSample :: FilePath -> Handler [Value]
checkpointsSample dbPath =
  liftIO $ withReadConnection dbPath $ \conn ->
    map pointJson <$> sampleCheckpoints conn

-- | How @GET \/checkpoints\/{slot-no}@ matches the requested slot. From kupo's
-- @?strict@: exact by request, at-or-before by default — the default exists to
-- find a usable ancestor of any slot, e.g. for rollback detection.
data SlotMatch = ExactSlot | AtOrBefore

-- | @GET \/checkpoints\/{slot-no}@ — one point, or @null@ when nothing matches.
checkpointBySlot :: FilePath -> Int64 -> Bool -> Handler Value
checkpointBySlot dbPath slot strictFlag =
  liftIO $ withReadConnection dbPath $ \conn -> do
    rows <- case (if strictFlag then ExactSlot else AtOrBefore) of
      ExactSlot ->
        query conn "SELECT slot_no, header_hash FROM checkpoints WHERE slot_no = ?" (Only slot)
      AtOrBefore ->
        query
          conn
          "SELECT slot_no, header_hash FROM checkpoints \
          \WHERE slot_no <= ? ORDER BY slot_no DESC LIMIT 1"
          (Only slot)
    pure $ case rows of
      point : _ -> pointJson point
      [] -> Null

-- | A chain point in kupo's wire shape.
pointJson :: (Int64, ByteString) -> Value
pointJson (slot, hash) = object ["slot_no" .= slot, "header_hash" .= hexText hash]
