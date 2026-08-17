{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/health@ and @\/metrics@ — operational state.
module Cardano.Server.Api.Health
  ( HealthAPI
  , healthServer
  )
where

import Cardano.Api
  ( ChainTip (ChainTip, ChainTipAtGenesis)
  , ConsensusModeParams (CardanoModeParams)
  , EpochSlots (EpochSlots)
  , LocalNodeConnectInfo (..)
  , NetworkId
  , SocketPath
  , getLocalChainTip
  )

import Cardano.Server.Api.Common (withReadConnection)
import Cardano.Slotting.Slot (unSlotNo)

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null, String), object, (.=))
import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Version (showVersion)
import Database.SQLite.Simple (Only (Only), query, query_)

import Paths_cardano_sieve qualified as Paths
import Servant (Get, Handler, JSON, PlainText, Server, (:<|>) ((:<|>)), (:>))

-- | Operational state, kupo's field names. @\/health@ answers JSON; @\/metrics@
-- answers the same facts in Prometheus exposition format (one divergence from
-- kupo, which content-negotiates both on either path).
type HealthAPI =
  "health" :> Get '[JSON] Value
    :<|> "metrics" :> Get '[PlainText] Text

-- | Both routes.
healthServer :: Maybe (SocketPath, NetworkId) -> FilePath -> Server HealthAPI
healthServer node dbPath = healthJson node dbPath :<|> healthMetrics node dbPath

-- | Everything the health endpoints report, gathered once per request.
data HealthSnapshot = HealthSnapshot
  { hsCheckpoint :: Maybe Int64
  -- ^ Newest indexed slot, from @checkpoints@ — advances live under sync+serve.
  , hsNodeTip :: Maybe Int64
  -- ^ The node's tip slot, asked of the node itself ('getLocalChainTip') when a
  -- socket was configured. 'Nothing' in serve-only mode, and when the node does
  -- not answer — which downgrades 'hsConnected' too, exactly what monitoring
  -- should see when the node dies out from under a sync+serve.
  , hsConnected :: Bool
  , hsIndexesInstalled :: Bool
  , hsPolicyIndexDerived :: Bool
  -- ^ Whether the deferred completion step ('buildIndexesOn') has run. Probed
  -- via @policiesByPolicyId@'s existence, NOT the row count: a range with no
  -- native assets legitimately derives an empty table. Until this is true,
  -- policy\/asset queries answer @[]@ vacuously and DELETE by policy refuses.
  , hsDatabaseBytes :: Int64
  }

-- | One probe for both health endpoints. Every read is a pragma or a point
-- lookup — no @count(*)@ scans, so the cost does not grow with the database.
healthSnapshot :: Maybe (SocketPath, NetworkId) -> FilePath -> IO HealthSnapshot
healthSnapshot node dbPath = do
  (cp, indexed, derived, bytes) <- withReadConnection dbPath $ \conn -> do
    cp <- query_ conn "SELECT max(slot_no) FROM checkpoints" :: IO [Only (Maybe Int64)]
    idx <- indexExists conn "unspentByAddress"
    pol <- indexExists conn "policiesByPolicyId"
    pages <- query_ conn "PRAGMA page_count" :: IO [Only Int64]
    pageSize <- query_ conn "PRAGMA page_size" :: IO [Only Int64]
    pure
      ( case cp of Only c : _ -> c; [] -> Nothing
      , idx
      , pol
      , product [n | Only n <- pages <> pageSize]
      )
  tip <- case node of
    Nothing -> pure Nothing
    Just (socket, network) -> do
      -- A short-lived node-to-client connection per request: local socket,
      -- milliseconds. kupo answers from an in-memory health record; a cached
      -- tip here is a later refinement alongside the connection pool.
      answer <- try (getLocalChainTip (connectInfo socket network)) :: IO (Either SomeException ChainTip)
      pure $ case answer of
        Right (ChainTip slot _ _) -> Just (fromIntegral (unSlotNo slot))
        Right ChainTipAtGenesis -> Just 0
        Left _ -> Nothing
  pure
    HealthSnapshot
      { hsCheckpoint = cp
      , hsNodeTip = tip
      , hsConnected = maybe False (const True) tip
      , hsIndexesInstalled = indexed
      , hsPolicyIndexDerived = derived
      , hsDatabaseBytes = bytes
      }
 where
  indexExists conn name = do
    rows <-
      query
        conn
        "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ?"
        (Only (name :: Text))
        :: IO [Only Int]
    pure (not (null rows))
  connectInfo socket network =
    LocalNodeConnectInfo
      { -- Byron-era slots-per-epoch, only used decoding Byron blocks; the tip
        -- query never does. Same constant the indexer uses.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = network
      , localNodeSocketPath = socket
      }

-- | @GET \/health@ — kupo's field names, sieve's honesty about them.
-- @seconds_since_last_block@ is always null (sieve keeps no in-memory clock of
-- block arrival), and @network_synchronization@ is the checkpoint\/tip slot
-- ratio — kupo computes its own against wall-clock time via network parameters,
-- which sieve does not carry.
healthJson :: Maybe (SocketPath, NetworkId) -> FilePath -> Handler Value
healthJson node dbPath = do
  hs <- liftIO (healthSnapshot node dbPath)
  pure $
    object
      [ "connection_status" .= String (if hsConnected hs then "connected" else "disconnected")
      , "most_recent_checkpoint" .= hsCheckpoint hs
      , "most_recent_node_tip" .= hsNodeTip hs
      , "seconds_since_last_block" .= Null
      , "network_synchronization" .= synchronization hs
      , "configuration"
          .= object
            [ "indexes" .= String (if hsIndexesInstalled hs then "installed" else "deferred")
            , "policy_index" .= String (if hsPolicyIndexDerived hs then "derived" else "pending")
            ]
      , "version" .= showVersion Paths.version
      ]

-- | @GET \/metrics@ — the same snapshot in Prometheus exposition format.
healthMetrics :: Maybe (SocketPath, NetworkId) -> FilePath -> Handler Text
healthMetrics node dbPath = do
  hs <- liftIO (healthSnapshot node dbPath)
  let gauge name v = "# TYPE sieve_" <> name <> " gauge\nsieve_" <> name <> " " <> v <> "\n"
  pure $
    mconcat
      [ gauge "connection_status" (if hsConnected hs then "1" else "0")
      , maybe "" (gauge "most_recent_checkpoint" . pack . show) (hsCheckpoint hs)
      , maybe "" (gauge "most_recent_node_tip" . pack . show) (hsNodeTip hs)
      , maybe "" (gauge "network_synchronization" . pack . show) (synchronization hs)
      , gauge "indexes_installed" (if hsIndexesInstalled hs then "1" else "0")
      , gauge "policy_index_derived" (if hsPolicyIndexDerived hs then "1" else "0")
      , gauge "database_size_bytes" (pack (show (hsDatabaseBytes hs)))
      ]

-- | Checkpoint over node tip, both known, else null — how far behind the chain
-- this database is, as a ratio a dashboard can alert on.
synchronization :: HealthSnapshot -> Maybe Double
synchronization hs = do
  cp <- hsCheckpoint hs
  tip <- hsNodeTip hs
  if tip <= 0
    then Nothing
    else Just (fromIntegral (min cp tip) / fromIntegral tip)
