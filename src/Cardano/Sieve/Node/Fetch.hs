{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Follow the chain over the node-to-client ChainSync protocol (pipelined) and
-- print the block number of every block that is rolled forward.
--
-- This is the Phase-1 proof-of-life for the indexing loop described in ADR-020:
-- a single-threaded, pipelined ChainSync client with no application-level queue.
-- The buffering below the application (node send buffer, kernel, mux ingress)
-- provides flow control; pipelining depth is the only bound on in-flight data.
module Cardano.Sieve.Node.Fetch
  ( fetch
  )
where

import Cardano.Api
  ( BlockHeader (BlockHeader)
  , BlockInMode (BlockInMode)
  , BlockNo
  , ChainPoint (ChainPoint, ChainPointAtGenesis)
  , ChainTip (ChainTip, ChainTipAtGenesis)
  , ConsensusModeParams (CardanoModeParams)
  , EpochSlots (EpochSlots)
  , LocalChainSyncClient (LocalChainSyncClientPipelined)
  , LocalNodeClientProtocols (..)
  , LocalNodeClientProtocolsInMode
  , LocalNodeConnectInfo (..)
  , NetworkId
  , SocketPath
  , connectToLocalNode
  , getBlockHeader
  , serialiseToRawBytesHexText
  )

import Cardano.Sieve.Node.Insert
  ( BlockHeaderRow (..)
  , DbHandle
  , closeDatabase
  , openDatabase
  , rollbackAbove
  , writeHeader
  )
import Cardano.Slotting.Block (unBlockNo)
import Cardano.Slotting.Slot (WithOrigin (At, Origin), unSlotNo)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( PipelineDecision (Collect)
  , pipelineDecisionMax
  )

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Word (Word16)
import Network.TypedProtocol.Core (Nat (Succ, Zero))

-- | Follow a local node's chain, writing each block header to SQLite (committing
-- every @batchSize@ headers) and printing its block number.
fetch :: SocketPath -> NetworkId -> FilePath -> Int -> IO ()
fetch socketPath networkId dbPath batchSize =
  bracket
    (openDatabase dbPath batchSize)
    closeDatabase
    (\dbHandle -> connectToLocalNode connectInfo (protocols dbHandle))
 where
  connectInfo :: LocalNodeConnectInfo
  connectInfo =
    LocalNodeConnectInfo
      { -- Byron-era slots-per-epoch, used only to decode Byron-era blocks. The
        -- network is selected by 'localNodeNetworkId' below, not this: 21600 is
        -- the value every network with Byron history uses, and post-Byron
        -- testnets (e.g. cardano-testnet's Conway chain) carry no Byron blocks,
        -- so it is never exercised there — which is why a magic-42 testnet syncs
        -- fine with it. cardano-cli hardcodes the same value.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = socketPath
      }

  protocols :: DbHandle -> LocalNodeClientProtocolsInMode
  protocols dbHandle =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClientPipelined (chainSyncClient dbHandle)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | The pipelined ChainSync client. It keeps up to 'maxInFlight' requests in
-- flight, collecting a response whenever 'pipelineDecisionMax' says to, and on
-- every roll-forward writes the header via the 'DbHandle' and prints the
-- block number.
chainSyncClient
  :: DbHandle
  -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
chainSyncClient dbHandle =
  CSP.ChainSyncClientPipelined (pure (clientIdle Origin Origin Zero))
 where
  -- Initial pipelining depth (ADR-020 Decision 1: the only bound on this path).
  maxInFlight :: Word16
  maxInFlight = 50

  -- Decide whether to pipeline another request or collect a pending response.
  -- The two 'WithOrigin BlockNo' tips feed 'pipelineDecisionMax' so it can back
  -- off pipelining as we approach the server's tip.
  clientIdle
    :: WithOrigin BlockNo
    -- \^ Our tip (last block we have seen)
    -> WithOrigin BlockNo
    -- \^ The server's reported tip
    -> Nat n
    -- \^ Requests currently in flight
    -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
  clientIdle clientTip serverTip n =
    case pipelineDecisionMax maxInFlight n clientTip serverTip of
      -- 'Collect' is GADT-refined to @n ~ 'S' n1@, so a request is guaranteed to
      -- be in flight and this 'Succ' match is total (a 'Zero' case would be
      -- inaccessible by types).
      Collect -> case n of
        Succ predN -> CSP.CollectResponse Nothing (clientNext predN)
      _ ->
        CSP.SendMsgRequestNextPipelined
          (pure ())
          (clientIdle clientTip serverTip (Succ n))

  -- Handle the next response. We recompute the tips from the block/tip carried
  -- by the message rather than threading them through the pipeline.
  clientNext
    :: Nat n
    -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \(BlockInMode _ block) serverTip -> do
          let BlockHeader slotNo hash blockNo = getBlockHeader block
          -- Persist the header as one row — block and slot numbers as integers,
          -- the header hash as lowercase base16 text; buffered into the open
          -- transaction and committed per the batch size.
          writeHeader dbHandle $
            BlockHeaderRow
              { rowBlockNo = fromIntegral (unBlockNo blockNo)
              , rowSlotNo = fromIntegral (unSlotNo slotNo)
              , rowHash = serialiseToRawBytesHexText hash
              }
          putStrLn ("block " <> show blockNo)
          pure (clientIdle (At blockNo) (fromChainTip serverTip) n)
      , CSP.recvMsgRollBackward = \point serverTip -> do
          -- Drop persisted headers newer than the rollback point so the table
          -- stays consistent with the chain. We do not track our own block
          -- history here, so we forget our tip and let pipelining ramp back up
          -- from Origin.
          rollbackAbove dbHandle (chainPointSlot point)
          pure (clientIdle Origin (fromChainTip serverTip) n)
      }

  fromChainTip :: ChainTip -> WithOrigin BlockNo
  fromChainTip = \case
    ChainTipAtGenesis -> Origin
    ChainTip _slotNo _hash blockNo -> At blockNo

  -- The slot of a rollback point, or 'Nothing' for a rollback to genesis.
  chainPointSlot :: ChainPoint -> Maybe Int64
  chainPointSlot = \case
    ChainPointAtGenesis -> Nothing
    ChainPoint slotNo _hash -> Just (fromIntegral (unSlotNo slotNo))
