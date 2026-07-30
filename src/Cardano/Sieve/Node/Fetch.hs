{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Follow the chain over the node-to-client ChainSync protocol (pipelined),
-- sieve each block's outputs, and write the matches to SQLite.
--
-- The indexing loop (ADR-020) is a single-threaded, pipelined ChainSync client
-- with no application-level queue. The buffering below the application (node
-- send buffer, kernel, mux ingress) provides flow control; pipelining depth is
-- the only bound on in-flight data.
--
-- There are two clients, both starting from a configurable point (@--since@,
-- genesis by default) and sharing 'sieveBlock' for the roll-forward body:
-- 'followingClient' streams forever; 'boundedClient' stops after a given slot
-- (@--until@), draining the requests still in flight before it finishes.
module Cardano.Sieve.Node.Fetch
  ( fetch
  , fetchBounded
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
  , serialiseToRawBytes
  )

import Cardano.Sieve.Node.Decode (selectedStored, spentInputs)
import Cardano.Sieve.Node.Insert
  ( DbHandle
  , applyBlock
  , buildIndexesOn
  , closeDatabase
  , openDatabase
  , rollbackAbove
  )
import Cardano.Sieve.Selector (Selector)
import Cardano.Slotting.Slot (SlotNo, WithOrigin (At, Origin), unSlotNo)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( PipelineDecision (Collect)
  , pipelineDecisionMax
  )

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Word (Word16)
import Network.TypedProtocol.Core (Nat (Succ, Zero))

-- | Follow a local node's chain forever, starting at @since@ (genesis by
-- default): sieve each block's outputs against the selectors and persist the
-- matches to SQLite, committing every @batchSize@ outputs.
fetch
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> [Selector]
  -> ChainPoint
  -> IO ()
fetch socketPath networkId dbPath batchSize selectors since =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    (\dbHandle -> followingClient dbHandle selectors since)

-- | As 'fetch', but index only from @since@ up to and including @untilSlot@,
-- then stop. For bounded backfills and benchmark runs.
fetchBounded
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> [Selector]
  -> ChainPoint
  -> SlotNo
  -> IO ()
fetchBounded socketPath networkId dbPath batchSize selectors since untilSlot =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    (\dbHandle -> boundedClient dbHandle selectors since untilSlot)

-- | Open the database, connect to the local node, and drive the given
-- pipelined ChainSync client, flushing the database on exit. The bounded and
-- following entry points differ only in which client they hand to this.
runSync
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> (DbHandle -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ())
  -> IO ()
runSync socketPath networkId dbPath batchSize mkClient =
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
      { localChainSyncClient = LocalChainSyncClientPipelined (mkClient dbHandle)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | Sieve one block's outputs against the selectors, persist the matches, and
-- record spends of any tracked outputs the block's transactions consumed;
-- print a one-line summary. Returns the block's header — the bounded client
-- needs its slot to detect the stop point.
sieveBlock :: DbHandle -> [Selector] -> BlockInMode -> IO BlockHeader
sieveBlock dbHandle selectors blockInMode@(BlockInMode _ block) = do
  let header = getBlockHeader block
      BlockHeader slotNo hash blockNo = header
      selected = selectedStored selectors blockInMode
      spent = spentInputs blockInMode
  applyBlock
    dbHandle
    (fromIntegral (unSlotNo slotNo))
    (serialiseToRawBytes hash)
    selected
    spent
  putStrLn
    ( "block "
        <> show blockNo
        <> " ("
        <> show (length selected)
        <> " selected, "
        <> show (length spent)
        <> " inputs)"
    )
  pure header

-- | A pipelined ChainSync client that finds its intersection at @since@ and
-- then streams the chain forever, sieving each roll-forward and rewinding on
-- rollback. This is the original follow-the-tip behaviour, generalised only by
-- the configurable start point.
followingClient
  :: DbHandle
  -> [Selector]
  -> ChainPoint
  -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
followingClient dbHandle selectors since =
  CSP.ChainSyncClientPipelined $ do
    built <- newIORef False
    pure (clientIntersect built)
 where
  maxInFlight :: Word16
  maxInFlight = 50

  clientIntersect built =
    CSP.SendMsgFindIntersect [since] $
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_point serverTip ->
            pure (clientIdle built Origin (fromChainTip serverTip) Zero)
        , CSP.recvMsgIntersectNotFound = \_serverTip ->
            fail ("--since point not on the node's chain: " <> show since)
        }

  clientIdle
    :: IORef Bool
    -> WithOrigin BlockNo
    -> WithOrigin BlockNo
    -> Nat n
    -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
  clientIdle built clientTip serverTip n =
    case pipelineDecisionMax maxInFlight n clientTip serverTip of
      Collect -> case n of
        Succ predN -> CSP.CollectResponse Nothing (clientNext built predN)
      _ ->
        CSP.SendMsgRequestNextPipelined
          (pure ())
          (clientIdle built clientTip serverTip (Succ n))

  clientNext :: IORef Bool -> Nat n -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext built n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \blockInMode serverTip -> do
          BlockHeader _ _ blockNo <- sieveBlock dbHandle selectors blockInMode
          let tip = fromChainTip serverTip
          -- On first catching the node's tip, build the deferred query indexes
          -- once: bulk catch-up ran index-free, and from here tip updates are
          -- small increments on the indexed tables.
          when (At blockNo >= tip) $ do
            done <- readIORef built
            unless done $ do
              buildIndexesOn dbHandle
              writeIORef built True
          pure (clientIdle built (At blockNo) tip n)
      , CSP.recvMsgRollBackward = \point serverTip -> do
          rollbackAbove dbHandle (chainPointSlot point)
          pure (clientIdle built Origin (fromChainTip serverTip) n)
      }

-- | A pipelined ChainSync client that indexes from @since@ up to and including
-- @untilSlot@, then stops. On reaching the bound it stops issuing new requests,
-- drains the responses still in flight ('SendMsgDone' is only legal with none
-- outstanding), and finishes — so the run ends with a clean commit via
-- 'fetch''s bracket. Blocks that arrive during the drain are past the bound and
-- discarded.
--
-- Rollbacks below an already-reached bound are not re-crossed here (the drain
-- ignores them); on an immutable historical range none occur, which is the
-- intended use.
boundedClient
  :: DbHandle
  -> [Selector]
  -> ChainPoint
  -> SlotNo
  -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
boundedClient dbHandle selectors since untilSlot =
  CSP.ChainSyncClientPipelined (pure clientIntersect)
 where
  maxInFlight :: Word16
  maxInFlight = 50

  clientIntersect =
    CSP.SendMsgFindIntersect [since] $
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_point serverTip ->
            pure (clientIdle False Origin (fromChainTip serverTip) Zero)
        , CSP.recvMsgIntersectNotFound = \_serverTip ->
            fail ("--since point not on the node's chain: " <> show since)
        }

  -- The 'Bool' is whether we have reached the bound and are draining: no new
  -- requests are issued, outstanding responses are collected, and once none
  -- remain we are done.
  clientIdle
    :: Bool
    -> WithOrigin BlockNo
    -> WithOrigin BlockNo
    -> Nat n
    -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
  clientIdle draining clientTip serverTip n
    | draining =
        case n of
          Zero -> CSP.SendMsgDone ()
          Succ predN -> CSP.CollectResponse Nothing (clientNext True predN)
    | otherwise =
        case pipelineDecisionMax maxInFlight n clientTip serverTip of
          Collect -> case n of
            Succ predN -> CSP.CollectResponse Nothing (clientNext False predN)
          _ ->
            CSP.SendMsgRequestNextPipelined
              (pure ())
              (clientIdle False clientTip serverTip (Succ n))

  clientNext :: Bool -> Nat n -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext draining n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \blockInMode serverTip ->
          if draining
            then pure (clientIdle True Origin (fromChainTip serverTip) n)
            else do
              -- Peek the slot BEFORE indexing: a block past the bound must not
              -- be written. --until is inclusive (kupo's <= semantics), so index
              -- iff slot <= untilSlot; the first block beyond the bound flips us
              -- to draining without being indexed. A block landing exactly on
              -- untilSlot is indexed and starts the drain in the same step.
              let BlockHeader slotNo _ blockNo =
                    case blockInMode of BlockInMode _ block -> getBlockHeader block
              if slotNo > untilSlot
                then pure (clientIdle True Origin (fromChainTip serverTip) n)
                else do
                  _ <- sieveBlock dbHandle selectors blockInMode
                  pure (clientIdle (slotNo >= untilSlot) (At blockNo) (fromChainTip serverTip) n)
      , CSP.recvMsgRollBackward = \point serverTip ->
          if draining
            then pure (clientIdle True Origin (fromChainTip serverTip) n)
            else do
              rollbackAbove dbHandle (chainPointSlot point)
              pure (clientIdle False Origin (fromChainTip serverTip) n)
      }

-- | The server tip as a 'WithOrigin' block number, for 'pipelineDecisionMax'.
fromChainTip :: ChainTip -> WithOrigin BlockNo
fromChainTip = \case
  ChainTipAtGenesis -> Origin
  ChainTip _slotNo _hash blockNo -> At blockNo

-- | The slot of a rollback point, or 'Nothing' for a rollback to genesis.
chainPointSlot :: ChainPoint -> Maybe Int64
chainPointSlot = \case
  ChainPointAtGenesis -> Nothing
  ChainPoint slotNo _hash -> Just (fromIntegral (unSlotNo slotNo))
