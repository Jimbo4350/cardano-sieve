{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | One-shot block retrieval over the node-to-client ChainSync protocol, for
-- @GET \/metadata@: metadata is never stored (kupo stores none either — the
-- chain itself already indexes it by slot), so each request opens a short-lived
-- connection and walks from a known ancestor to the block it wants.
--
-- kupo's equivalent (@FetchBlock.Node@) keeps one long-lived connection and
-- takes exactly the block after the intersection; this one connects per
-- request — the same trade 'Cardano.Server.Http.healthSnapshot' makes for the
-- node tip, and the same later refinement (a pooled connection) applies —
-- and walks until the target slot, because sieve's checkpoints, though also
-- stored per applied block, may start at a @--since@ rather than genesis.
module Cardano.Sieve.Node.FetchBlock
  ( fetchBlockAtSlot
  )
where

import Cardano.Api
  ( BlockHeader (BlockHeader)
  , BlockInMode (BlockInMode)
  , ChainPoint
  , ChainTip
  , ConsensusModeParams (CardanoModeParams)
  , EpochSlots (EpochSlots)
  , LocalChainSyncClient (LocalChainSyncClient)
  , LocalNodeClientProtocols (..)
  , LocalNodeClientProtocolsInMode
  , LocalNodeConnectInfo (..)
  , NetworkId
  , SocketPath
  , connectToLocalNode
  , getBlockHeader
  )

import Cardano.Slotting.Slot (SlotNo)
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | The first block at or after @target@, walking forward from @ancestor@ —
-- a point the node should recognise (a stored checkpoint, or genesis).
--
-- 'Nothing' when the node does not recognise the ancestor, or when the chain
-- rolls back mid-walk: both mean a rollback won a race against this request,
-- and both get kupo's \"no ancestor\" answer upstream. The walk is normally a
-- single block — the at-or-before ancestor of @target − 1@ is the block
-- immediately preceding @target@ wherever checkpoints are dense — and a
-- target past the node's tip blocks until the chain reaches it, exactly as
-- kupo's fetch does.
--
-- Note the contract inherited from kupo: this returns the first block AT OR
-- PAST the target, with no check that a block sits exactly at it. A slot
-- nobody minted in answers with the next block's content — which is why the
-- endpoint reports the block's actual header hash for the client to verify.
fetchBlockAtSlot :: SocketPath -> NetworkId -> ChainPoint -> SlotNo -> IO (Maybe BlockInMode)
fetchBlockAtSlot socketPath networkId ancestor target = do
  result <- newIORef Nothing
  connectToLocalNode connectInfo (protocols result)
  readIORef result
 where
  connectInfo :: LocalNodeConnectInfo
  connectInfo =
    LocalNodeConnectInfo
      { -- Byron-era slots-per-epoch, only used decoding Byron blocks; see the
        -- note on 'Cardano.Sieve.Node.Fetch.runSync' for why the constant is
        -- safe everywhere.
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = socketPath
      }

  protocols :: IORef (Maybe BlockInMode) -> LocalNodeClientProtocolsInMode
  protocols result =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient (client result)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  client :: IORef (Maybe BlockInMode) -> CS.ChainSyncClient BlockInMode ChainPoint ChainTip IO ()
  client result =
    CS.ChainSyncClient . pure $
      CS.SendMsgFindIntersect
        [ancestor]
        CS.ClientStIntersect
          { CS.recvMsgIntersectFound = \_point _tip ->
              CS.ChainSyncClient (pure (next settling))
          , CS.recvMsgIntersectNotFound = \_tip ->
              CS.ChainSyncClient (pure (CS.SendMsgDone ()))
          }
   where
    next = CS.SendMsgRequestNext (pure ())

    -- The node's first reply after a found intersection is a rollback TO that
    -- intersection; the walk proper starts on the reply after it. A forward
    -- block here would be a protocol violation — stepped on rather than
    -- crashed on, since a block is a block.
    settling =
      CS.ClientStNext
        { CS.recvMsgRollForward = \block _tip -> step block
        , CS.recvMsgRollBackward = \_point _tip ->
            CS.ChainSyncClient (pure (next walking))
        }

    walking =
      CS.ClientStNext
        { CS.recvMsgRollForward = \block _tip -> step block
        , -- A rollback mid-walk: the chain reorganised under the request.
          -- Give up — stitching blocks from two forks into one answer is
          -- worse than asking the client to retry.
          CS.recvMsgRollBackward = \_point _tip ->
            CS.ChainSyncClient (pure (CS.SendMsgDone ()))
        }

    step block@(BlockInMode _ blk) = CS.ChainSyncClient $
      case getBlockHeader blk of
        BlockHeader slot _ _
          | slot >= target -> do
              writeIORef result (Just block)
              pure (CS.SendMsgDone ())
          | otherwise -> pure (next walking)
