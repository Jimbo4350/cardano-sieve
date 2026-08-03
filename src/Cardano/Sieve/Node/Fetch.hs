-- DataKinds: 'collectFlushingWhenIdle' names the pipeline depth it returns at,
-- @(S n)@, promoting typed-protocols' 'N' constructor to the type level.
{-# LANGUAGE DataKinds #-}
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

    -- * Pipelining and the write cadence
    -- $pipelining
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

import Cardano.Sieve.Node.Decode (preimagesInBlock, selectedStored, spentInputs)
import Cardano.Sieve.Node.Insert
  ( DbHandle
  , RedeemerCapture
  , applyBlock
  , buildIndexesOn
  , closeDatabase
  , flushBatch
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
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Word (Word16)
import GHC.Clock (getMonotonicTime)
import Network.TypedProtocol.Core (N (S), Nat (Succ, Zero))
import Numeric (showFFloat)

-- | Follow a local node's chain forever, starting at @since@ (genesis by
-- default): sieve each block's outputs against the selectors and persist the
-- matches to SQLite, committing every @batchSize@ outputs.
fetch
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> IO ()
fetch socketPath networkId dbPath batchSize capture selectors since =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    (\dbHandle progress -> followingClient dbHandle progress capture selectors since)

-- | As 'fetch', but index only from @since@ up to and including @untilSlot@,
-- then stop. For bounded backfills and benchmark runs.
fetchBounded
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> SlotNo
  -> IO ()
fetchBounded socketPath networkId dbPath batchSize capture selectors since untilSlot =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    (\dbHandle progress -> boundedClient dbHandle progress capture selectors since untilSlot)

-- | Open the database, connect to the local node, and drive the given
-- pipelined ChainSync client, flushing the database on exit. The bounded and
-- following entry points differ only in which client they hand to this.
--
-- Brackets the run with progress reporting: a line up front so it is obvious the
-- sync started, a heartbeat every 'heartbeatSeconds' while it runs, and a closing
-- summary once the client finishes (which a bounded run does and a following one
-- does not).
runSync
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> ( DbHandle
       -> IORef Progress
       -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
     )
  -> IO ()
runSync socketPath networkId dbPath batchSize mkClient =
  bracket
    (openDatabase dbPath batchSize)
    closeDatabase
    ( \dbHandle -> do
        progress <- newProgress
        putStrLn
          ( "sync starting  db="
              <> dbPath
              <> "  batch-size="
              <> show batchSize
              <> "  (progress every "
              <> showFFloat (Just 0) heartbeatSeconds "s)"
          )
        connectToLocalNode connectInfo (protocols dbHandle progress)
        summarise progress
    )
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

  protocols :: DbHandle -> IORef Progress -> LocalNodeClientProtocolsInMode
  protocols dbHandle progress =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClientPipelined (mkClient dbHandle progress)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | Rolling counters behind the periodic sync heartbeat.
--
-- A bulk sync processes millions of blocks, so a line per block is unreadable
-- and costs real throughput in the hot loop (it is why the benchmark used to
-- discard sieve's output wholesale). Instead every roll-forward folds its work
-- into these counters — cheap, no I/O — and a line is emitted only once
-- 'heartbeatSeconds' have passed.
data Progress = Progress
  { pgStartedAt :: !Double
  -- ^ Monotonic seconds when the sync began; the basis for @elapsed@.
  , pgReportedAt :: !Double
  -- ^ Monotonic seconds when the last line was emitted. Also the left edge of
  -- the window the reported block rate is computed over.
  , pgBlocks :: !Int
  -- ^ Blocks rolled forward since the start.
  , pgOutputs :: !Int
  -- ^ Matched outputs written since the start.
  , pgSpends :: !Int
  -- ^ Spends recorded since the start.
  , pgBlocksAtReport :: !Int
  -- ^ 'pgBlocks' as of the last emitted line, so the rate is the /recent/ rate
  -- rather than a start-to-now average that hides a slowdown.
  }

-- | Emit at most one progress line per this many seconds.
heartbeatSeconds :: Double
heartbeatSeconds = 5

newProgress :: IO (IORef Progress)
newProgress = do
  now <- getMonotonicTime
  newIORef (Progress now now 0 0 0 0)

-- | Fold one block's work into the counters, emitting a progress line if the
-- heartbeat interval has elapsed. One clock read per block on the common path.
--
-- @target@ is the slot the sync is heading for, when known — @--until@ for a
-- bounded run, the server's tip for a following one — and drives the percentage.
tick :: IORef Progress -> SlotNo -> Maybe SlotNo -> Int -> Int -> IO ()
tick ref slotNo target outputs spends = do
  now <- getMonotonicTime
  pg <- readIORef ref
  let folded =
        pg
          { pgBlocks = pgBlocks pg + 1
          , pgOutputs = pgOutputs pg + outputs
          , pgSpends = pgSpends pg + spends
          }
  if now - pgReportedAt folded < heartbeatSeconds
    then writeIORef ref folded
    else do
      writeIORef
        ref
        folded{pgReportedAt = now, pgBlocksAtReport = pgBlocks folded}
      putStrLn (progressLine folded now slotNo target)

-- | The heartbeat line, e.g.
--
-- > syncing  slot=1284213/3999989  32.1%  1843 blk/s  blocks=61204  outputs=418337  spends=205118  elapsed=35s
progressLine :: Progress -> Double -> SlotNo -> Maybe SlotNo -> String
progressLine pg now slotNo target =
  "syncing  slot="
    <> show (unSlotNo slotNo)
    <> maybe "" (\t -> "/" <> show (unSlotNo t) <> percent t) target
    <> "  "
    <> show (round rate :: Int)
    <> " blk/s  blocks="
    <> show (pgBlocks pg)
    <> "  outputs="
    <> show (pgOutputs pg)
    <> "  spends="
    <> show (pgSpends pg)
    <> "  elapsed="
    <> showFFloat (Just 0) (now - pgStartedAt pg) "s"
 where
  -- Guard the divisor: two blocks can share a clock reading.
  window = max 1e-6 (now - pgReportedAt pg)
  rate = fromIntegral (pgBlocks pg - pgBlocksAtReport pg) / window :: Double
  percent t
    | unSlotNo t == 0 = ""
    | otherwise =
        "  "
          <> showFFloat
            (Just 1)
            (100 * fromIntegral (unSlotNo slotNo) / fromIntegral (unSlotNo t) :: Double)
            "%"

-- | The closing line when a bounded sync finishes.
summarise :: IORef Progress -> IO ()
summarise ref = do
  now <- getMonotonicTime
  pg <- readIORef ref
  let secs = max 1e-6 (now - pgStartedAt pg)
  putStrLn
    ( "sync done  blocks="
        <> show (pgBlocks pg)
        <> "  outputs="
        <> show (pgOutputs pg)
        <> "  spends="
        <> show (pgSpends pg)
        <> "  elapsed="
        <> showFFloat (Just 1) secs "s"
        <> "  avg="
        <> show (round (fromIntegral (pgBlocks pg) / secs) :: Int)
        <> " blk/s"
    )

-- | Sieve one block's outputs against the selectors, persist the matches, and
-- record spends of any tracked outputs the block's transactions consumed;
-- fold the work into the progress counters. Returns the block's header — the
-- bounded client needs its slot to detect the stop point.
sieveBlock
  :: DbHandle
  -> IORef Progress
  -> RedeemerCapture
  -> [Selector]
  -> Maybe SlotNo
  -> BlockInMode
  -> IO BlockHeader
sieveBlock dbHandle progress capture selectors target blockInMode@(BlockInMode _ block) = do
  let header = getBlockHeader block
      BlockHeader slotNo hash _blockNo = header
      selected = selectedStored selectors blockInMode
      spent = spentInputs capture blockInMode
  applyBlock
    dbHandle
    (fromIntegral (unSlotNo slotNo))
    (serialiseToRawBytes hash)
    selected
    spent
    (preimagesInBlock blockInMode)
  tick progress slotNo target (length selected) (length spent)
  pure header

-- | Whether the deferred query indexes have been built yet. The follower builds
-- them once, the first time it reaches the node's tip; a named state reads
-- better than a bare 'Bool' at the roll-forward guard.
data IndexState = IndexesPending | IndexesBuilt

-- | Which half of its life 'boundedClient' is in.
--
-- Two phases rather than one, because @SendMsgDone@ is only legal with nothing
-- in flight: on reaching @--until@ the client is still owed the ~50 responses it
-- pipelined ahead, and must collect them all before it may finish.
data BoundedPhase
  = -- | Before the bound: consult 'pipelineDecisionMax', request more, and write
    -- what arrives.
    Indexing
  | -- | Past the bound: request nothing, discard what arrives, and finish once
    -- the pipeline is empty.
    Draining

-- $pipelining
--
-- 'pipelineDecisionMax' lives in @ouroboros-network@ and decides, at every step,
-- whether to send another request or take delivery of one. Its rule is small but
-- the whole write cadence hangs off it, so it is restated here rather than left in
-- another repository:
--
-- @
--     Zero    | clientTip == serverTip  -> Request     -- caught up: block for one
--             | otherwise               -> Pipeline
--
--     Succ{}  | clientTip + n >= serverTip
--               || n >= maxInFlight     -> Collect     -- take delivery of ONE
--             | otherwise               -> Pipeline    -- ask for another
-- @
--
-- @n@ is the number of requests SENT AND NOT YET COLLECTED. It is not the number
-- of responses that have arrived — those are two different quantities, and the
-- distinction is the one that matters for 'collectFlushingWhenIdle'.
--
-- Two independent triggers for @Collect@:
--
--   * __@n >= maxInFlight@__ — the pipeline is full. This is the bulk-sync case.
--   * __@clientTip + n >= serverTip@__ — the blocks already asked for would carry
--     us to the node's tip, so there is no point asking for more. This is what
--     makes the client /drain/ as it approaches the tip instead of sitting on 50
--     unanswered requests.
--
-- The consequence worth internalising: far from the tip the client settles into
-- collect-one, request-one at @n = maxInFlight@, so @Collect@ comes back on
-- roughly every other call — once per block. Near the tip the second trigger fires
-- at @n = 1@, so @n@ oscillates 0..1. Either way @Collect@ is frequent; it is NOT
-- the thing that rations commits.
--
-- Note also that @Request@ — the non-pipelined, blocking form used when caught up
-- — is not handled distinctly below: the @_@ branch treats it as @Pipeline@. That
-- works, but ignores an explicit \"you are at the tip\" signal from the protocol.

-- | Build the \"collect one pipelined response\" instruction, spelling out /two/
-- alternatives for the driver to choose between:
--
--   * the response is already here — process it, the ordinary path;
--   * it is not here — COMMIT the rows written so far, instead of blocking with
--     them left uncommitted.
--
-- This function does not collect anything and does not commit anything. It
-- returns a value describing both alternatives; the driver runs one of them
-- later. That indirection is the whole of ADR-020 Decision 2, the /idle flush/,
-- and it is the part most likely to be misread — so:
--
-- == Two separate decisions, and the one that matters is not ours
--
-- It is easy to read this as \"we flush every time 'pipelineDecisionMax' says
-- 'Collect'\". It is not. There are two decisions, taken by different parties at
-- different times:
--
--   1. WE decide, via 'pipelineDecisionMax', /that we will take delivery/ of a
--      response. That is the @Collect@ branch in 'clientIdle'. During bulk sync
--      it happens once per block (see the note on 'pipelineDecisionMax'), so
--      this function is CALLED once per block.
--
--   2. THE DRIVER then decides /which branch of the value we built here to run/,
--      by asking a completely different question: has the next response actually
--      arrived? We never see that question; we only supply both answers up front.
--
-- So being called is not flushing. The flush is on the branch taken only when
-- the answer to (2) is no.
--
-- == Which branch runs
--
-- @
-- CollectResponse (Just idleAction) next
--                       │            │
--    response NOT here ─┘            └─ response already buffered: the
--    run idleAction instead of          ordinary path, no flush
--    blocking
-- @
--
-- typed-protocols states the contract: \"Since presenting the first choice is
-- optional, this allows expressing both a blocking collect and a non-blocking
-- collect.\" Passing 'Nothing' means \"block until it arrives\"; passing 'Just'
-- means \"if it has not arrived, do this instead\".
--
-- == What an empty pipeline implies
--
-- Nothing buffered, with requests outstanding, means the node has given us
-- everything it currently has. Two ways to get there:
--
--   * __At the tip__ — the common case. We asked for the next block and it does
--     not exist yet. Outstanding requests sit near 1 rather than 50, because
--     'pipelineDecisionMax' stops pipelining past the server's tip. Every block
--     therefore commits on its own, and a query sees the chain as of the last
--     block instead of the last 'dbBatchSize' boundary.
--
--   * __Starved during bulk sync__ — the node failed to deliver even the oldest
--     of ~50 outstanding requests. Should be rare against a local node serving
--     from disk. If it is NOT rare, bulk sync degrades towards a commit per
--     block, which is the expensive end of the batch-size curve (41.7s versus
--     31.6s over @origin..2,000,000@). This is measured, not assumed — see the
--     verification task; until then treat the bulk-sync claim as unconfirmed.
--
-- 'flushBatch' is a no-op when nothing is pending, so an idle flush with no
-- written rows costs one 'IORef' read.
--
-- == Why the inner collect blocks
--
-- After flushing we hand back @CollectResponse Nothing next@ — the /blocking/
-- form. Returning another non-blocking collect would re-enter this on the next
-- pass, find the batch already empty, and spin. Flush once, then wait.
collectFlushingWhenIdle
  :: DbHandle
  -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  -- ^ What to do with the response when it is (or once it is) here.
  -> CSP.ClientPipelinedStIdle (S n) BlockInMode ChainPoint ChainTip IO ()
collectFlushingWhenIdle dbHandle next =
  CSP.CollectResponse
    (Just (flushBatch dbHandle >> pure (CSP.CollectResponse Nothing next)))
    next

-- | A pipelined ChainSync client that finds its intersection at @since@ and
-- then streams the chain forever, sieving each roll-forward and rewinding on
-- rollback. This is the original follow-the-tip behaviour, generalised only by
-- the configurable start point.
followingClient
  :: DbHandle
  -> IORef Progress
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
followingClient dbHandle progress capture selectors since =
  CSP.ChainSyncClientPipelined $ do
    built <- newIORef IndexesPending
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
    :: IORef IndexState
    -> WithOrigin BlockNo
    -> WithOrigin BlockNo
    -> Nat n
    -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
  clientIdle built clientTip serverTip n =
    case pipelineDecisionMax maxInFlight n clientTip serverTip of
      Collect -> case n of
        Succ predN -> collectFlushingWhenIdle dbHandle (clientNext built predN)
      _ ->
        CSP.SendMsgRequestNextPipelined
          (pure ())
          (clientIdle built clientTip serverTip (Succ n))

  clientNext :: IORef IndexState -> Nat n -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext built n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \blockInMode serverTip -> do
          BlockHeader _ _ blockNo <-
            sieveBlock dbHandle progress capture selectors (chainTipSlot serverTip) blockInMode
          let tip = fromChainTip serverTip
          -- On first catching the node's tip, build the deferred query indexes
          -- once: bulk catch-up ran index-free, and from here tip updates are
          -- small increments on the indexed tables.
          when (At blockNo >= tip) $ do
            st <- readIORef built
            case st of
              IndexesBuilt -> pure ()
              IndexesPending -> do
                buildIndexesOn dbHandle
                writeIORef built IndexesBuilt
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
  -> IORef Progress
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> SlotNo
  -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
boundedClient dbHandle progress capture selectors since untilSlot =
  CSP.ChainSyncClientPipelined (pure clientIntersect)
 where
  maxInFlight :: Word16
  maxInFlight = 50

  clientIntersect =
    CSP.SendMsgFindIntersect [since] $
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_point serverTip ->
            pure (clientIdle Indexing Origin (fromChainTip serverTip) Zero)
        , CSP.recvMsgIntersectNotFound = \_serverTip ->
            fail ("--since point not on the node's chain: " <> show since)
        }

  clientIdle
    :: BoundedPhase
    -> WithOrigin BlockNo
    -> WithOrigin BlockNo
    -> Nat n
    -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
  clientIdle phase clientTip serverTip n =
    case phase of
      Draining ->
        case n of
          Zero -> CSP.SendMsgDone ()
          -- No idle flush while draining: these responses are past the bound and
          -- discarded, and 'runSync''s bracket commits what is pending on the way
          -- out.
          Succ predN -> CSP.CollectResponse Nothing (clientNext Draining predN)
      Indexing ->
        case pipelineDecisionMax maxInFlight n clientTip serverTip of
          Collect -> case n of
            Succ predN -> collectFlushingWhenIdle dbHandle (clientNext Indexing predN)
          _ ->
            CSP.SendMsgRequestNextPipelined
              (pure ())
              (clientIdle Indexing clientTip serverTip (Succ n))

  clientNext
    :: BoundedPhase -> Nat n -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext phase n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \blockInMode serverTip ->
          case phase of
            Draining -> pure (clientIdle Draining Origin (fromChainTip serverTip) n)
            Indexing -> do
              -- Peek the slot BEFORE indexing: a block past the bound must not
              -- be written. --until is inclusive (kupo's <= semantics), so index
              -- iff slot <= untilSlot; the first block beyond the bound flips us
              -- to draining without being indexed. A block landing exactly on
              -- untilSlot is indexed and starts the drain in the same step.
              let BlockHeader slotNo _ blockNo =
                    case blockInMode of BlockInMode _ block -> getBlockHeader block
              if slotNo > untilSlot
                then pure (clientIdle Draining Origin (fromChainTip serverTip) n)
                else do
                  _ <- sieveBlock dbHandle progress capture selectors (Just untilSlot) blockInMode
                  let next = if slotNo >= untilSlot then Draining else Indexing
                  pure (clientIdle next (At blockNo) (fromChainTip serverTip) n)
      , CSP.recvMsgRollBackward = \point serverTip ->
          case phase of
            Draining -> pure (clientIdle Draining Origin (fromChainTip serverTip) n)
            Indexing -> do
              rollbackAbove dbHandle (chainPointSlot point)
              pure (clientIdle Indexing Origin (fromChainTip serverTip) n)
      }

-- | The server tip as a 'WithOrigin' block number, for 'pipelineDecisionMax'.
fromChainTip :: ChainTip -> WithOrigin BlockNo
fromChainTip = \case
  ChainTipAtGenesis -> Origin
  ChainTip _slotNo _hash blockNo -> At blockNo

-- | The server tip's /slot/, for the progress percentage. Distinct from
-- 'fromChainTip', which keeps the block number: progress compares slot to slot,
-- and the two are not interchangeable on a chain with empty slots.
chainTipSlot :: ChainTip -> Maybe SlotNo
chainTipSlot = \case
  ChainTipAtGenesis -> Nothing
  ChainTip slotNo _hash _blockNo -> Just slotNo

-- | The slot of a rollback point, or 'Nothing' for a rollback to genesis.
chainPointSlot :: ChainPoint -> Maybe Int64
chainPointSlot = \case
  ChainPointAtGenesis -> Nothing
  ChainPoint slotNo _hash -> Just (fromIntegral (unSlotNo slotNo))
