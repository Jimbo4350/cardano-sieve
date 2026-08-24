{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Follow the chain over the node-to-client ChainSync protocol (pipelined),
-- sieve each block's outputs, and write the matches to SQLite.
--
-- The indexing loop is a single-threaded, pipelined ChainSync client
-- with no application-level queue. The buffering below the application (node
-- send buffer, kernel, mux ingress) provides flow control; pipelining depth is
-- the only bound on in-flight data. References: the mux ingress\/egress queues
-- are described in
-- <https://ouroboros-network.cardano.intersectmbo.org/network-mux/Network-Mux.html Network.Mux>
-- and the multiplexing chapter of
-- <https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec the network spec>.
--
-- There are two clients, both starting from a configurable point (@--since@,
-- genesis by default), and both handling a rolled-forward block the same way,
-- by calling 'sieveBlock' on it; they differ only in when they stop.
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
  , deserialiseFromRawBytes
  , getBlockHeader
  , proxyToAsType
  , serialiseToRawBytes
  )

import Cardano.Sieve.Node.Decode (datumsAndScriptsInBlock, spentInputs)
import Cardano.Sieve.Node.Encode (selectedStored)
import Cardano.Sieve.Node.Insert
  ( DbHandle (dbConn)
  , Durability (Durable, UnsafeBulk)
  , PolicyIndexing (DeferPolicies, MaintainPolicies)
  , RedeemerCapture
  , applyBlock
  , buildIndexesOn
  , closeDatabase
  , flushBatch
  , openDatabase
  , reconcileSelectors
  , rollbackAbove
  , sampleCheckpoints
  )
import Cardano.Sieve.Node.Progress
  ( Progress
  , commas
  , duration
  , heartbeatSeconds
  , logLine
  , newProgress
  , summarise
  , tick
  )
import Cardano.Sieve.Selector (Selector, selectorToText)
import Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin (At, Origin), unSlotNo)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( PipelineDecision (Collect)
  , pipelineDecisionMax
  )

import Control.Exception (bracket)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Clock (getMonotonicTime)
import Network.TypedProtocol.Core (N (S), Nat (Succ, Zero))

-- | Follow a local node's chain forever, starting at @since@ (genesis by
-- default): sieve each block's outputs against the selectors and persist the
-- matches to SQLite, committing every @batchSize@ outputs.
fetch
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> Durability
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> IO ()
fetch socketPath networkId dbPath batchSize durability redeemerCapture selectors since =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    durability
    selectors
    (\dbHandle progress active -> followingClient dbHandle progress redeemerCapture active since)

-- | Same as 'fetch', but index only from @since@ up to and including @untilSlot@,
-- then stop. For bounded backfills and benchmark runs.
fetchBounded
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> Int
  -> Durability
  -> RedeemerCapture
  -> [Selector]
  -> ChainPoint
  -> SlotNo
  -> IO ()
fetchBounded socketPath networkId dbPath batchSize durability redeemerCapture selectors since untilSlot =
  runSync
    socketPath
    networkId
    dbPath
    batchSize
    durability
    selectors
    (\dbHandle progress active -> boundedClient dbHandle progress redeemerCapture active since untilSlot)

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
  -> Durability
  -> [Selector]
  -> ( DbHandle
       -> IORef Progress
       -> [Selector]
       -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
     )
  -> IO ()
runSync socketPath networkId dbPath batchSize durability cliSelectors mkClient =
  bracket
    (openDatabase durability dbPath batchSize)
    closeDatabase
    ( \dbHandle -> do
        -- Before a single block is fetched: refuse to index into a database that
        -- was built with different selectors ('SelectorMismatch', fatal by
        -- design). When this run was given no --select at all, it reuses the
        -- selectors stored in the database, so a bare restart continues as before.
        selectors <- reconcileSelectors dbHandle cliSelectors
        progress <- newProgress
        logLine
          ( "sync starting  db "
              <> dbPath
              <> "  batch-size "
              <> commas batchSize
              <> "  (heartbeat every "
              <> duration heartbeatSeconds
              <> ")"
          )
        logLine ("indexing selectors: " <> describeSelectors selectors)
        case durability of
          UnsafeBulk ->
            logLine
              "bulk mode: journaling off for catch-up — a clean exit (Ctrl-C) is safe, \
              \but a crash means delete the database and resync"
          Durable -> pure ()
        connectToLocalNode connectInfo (protocols dbHandle progress selectors)
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

  protocols :: DbHandle -> IORef Progress -> [Selector] -> LocalNodeClientProtocolsInMode
  protocols dbHandle progress selectors =
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClientPipelined (mkClient dbHandle progress selectors)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | The selector set for the startup line.
--
-- Worth printing because the set is no longer necessarily the one on the command
-- line: an invocation with no @--select@ adopts whatever the database was built
-- with, so this is the only place the actual answer appears.
describeSelectors :: [Selector] -> String
describeSelectors = \case
  [] -> "none (nothing will be indexed)"
  xs -> intercalate ", " (map (T.unpack . selectorToText) xs)

-- | Sieve one block's outputs against the selectors, persist the matches, and
-- record spends of any tracked outputs the block's transactions consumed;
-- fold the work into the progress counters. Returns the block's header — the
-- bounded client needs its slot to detect the stop point.
sieveBlock
  :: DbHandle
  -> IORef Progress
  -> RedeemerCapture
  -> PolicyIndexing
  -> [Selector]
  -> Maybe SlotNo
  -> BlockInMode
  -> IO BlockHeader
sieveBlock dbHandle progress redeemerCapture policyIndexing selectors target blockInMode@(BlockInMode _ block) = do
  let header = getBlockHeader block
      BlockHeader slotNo hash _blockNo = header
      selected = selectedStored selectors blockInMode
      spent = spentInputs redeemerCapture blockInMode
  applyBlock
    dbHandle
    policyIndexing
    (fromIntegral (unSlotNo slotNo))
    (serialiseToRawBytes hash)
    selected
    spent
    (datumsAndScriptsInBlock blockInMode)
  tick progress slotNo target (length selected) (length spent)
  pure header

-- | Whether the deferred query indexes have been built yet. The follower builds
-- them once, the first time it reaches the node's tip.
data IndexState = IndexesPending | IndexesBuilt

-- | Which half of its life 'boundedClient' is in. The bound is the @--until@
-- slot, inclusive: the last slot the run indexes.
--
-- Two phases rather than one, because @SendMsgDone@ is only legal with nothing
-- in flight: on reaching the bound the client is still owed the ~50 responses it
-- pipelined ahead, and must collect them all before it may finish.
data BoundedPhase
  = -- | Before the bound: consult 'pipelineDecisionMax', request more, and write
    -- what arrives.
    Syncing
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
-- at @n = 1@, so @n@ oscillates 0..1. Either way @Collect@ is frequent — so if
-- @Collect@ meant commit, bulk sync would commit once per block. It does not:
-- collecting only takes delivery of a response. Whether the flush runs is the
-- driver's separate question — has the response even arrived? — and during bulk
-- sync the network runs ahead of SQLite, so the answer is almost always yes and
-- commits stay rationed by the batch counter. See 'collectFlushingWhenIdle'.
--
-- Note also that @Request@ — the non-pipelined, blocking form used when caught up
-- — is not handled distinctly below: the @_@ branch treats it as @Pipeline@,
-- which just pipelines one request and collects it straight back. Deliberate,
-- not an oversight — the client detects the tip from the empty pipeline instead —
-- but it does ignore an explicit \"you are at the tip\" signal from the protocol.

-- | Build the \"collect one pipelined response\" instruction, spelling out /two/
-- alternatives for the driver to choose between:
--
--   * the response is already here — process it, the ordinary path;
--   * it is not here — COMMIT the rows written so far, instead of blocking with
--     them left uncommitted.

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
-- == This is how the indexer knows it is at the tip
--
-- __There is no timer, no clock, no distance-to-tip calculation and no second
-- thread anywhere in this.__ An empty pipeline /is/ the at-the-tip signal, and it
-- costs nothing to observe because the driver has to answer \"has the response
-- arrived?\" on every collect regardless — we are only supplying what to do with
-- the \"no\" answer, which was previously thrown away as 'Nothing'.
--
-- That matters because the obvious alternatives are all worse:
--
--   * __A row-count cap alone__ \"leaves a near-empty batch open for minutes at
--     the tip (stale, unqueryable data)\" — blocks arrive every ~20s carrying a
--     handful of rows, so a 50,000-row cap is never reached and the transaction
--     stays open indefinitely.
--   * __A timer__ \"reintroduces a thread and a tunable\" — something has to wake
--     up and fire it, and someone has to pick the interval, and the interval is
--     wrong in both directions (too eager during bulk sync, too lazy at the tip).
--   * __A queue-fed writer thread__ needs exactly the queue and thread.
--
-- Flush-on-idle needs none of that: bulk sync and tip-following get different
-- behaviour out of the same rule, with no mode switch between them and no
-- constant to tune. Both regimes fall out below.
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
--     block.
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
    -- The idle branch: runs only when the driver finds no response buffered —
    -- nothing has arrived off the socket that we have not already consumed, so
    -- the outstanding requests are still unanswered: the tip, or a starved node.
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
followingClient dbHandle progress redeemerCapture selectors since =
  CSP.ChainSyncClientPipelined $ do
    built <- newIORef IndexesPending
    points <- startPoints dbHandle since
    pure (clientIntersect built points)
 where
  maxInFlight :: Word16
  maxInFlight = 50

  clientIntersect built points =
    CSP.SendMsgFindIntersect points $
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \point serverTip -> do
            logLine ("resuming from " <> describePoint point)
            pure (clientIdle built Origin (fromChainTip serverTip) Zero)
        , CSP.recvMsgIntersectNotFound = \_serverTip ->
            -- Only an explicit --since can land here: it is offered as the sole
            -- point, so the node can reject it. Otherwise 'startPoints' ends its
            -- candidate list with genesis, which is on every chain.
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
          -- The policies table is derived data (policy id -> output, redundant
          -- with the outputs' value bundles), so during catch-up no policies
          -- rows are written at all: 'buildIndexesOn' derives the whole table
          -- in one pass on reaching the tip, and only blocks after that
          -- maintain it per block. The handoff has no gap — the block that
          -- catches the tip is itself processed deferred, but its outputs are
          -- in the database before the derive scans, so the derive covers it.
          st0 <- readIORef built
          let policyIndexing = case st0 of
                IndexesPending -> DeferPolicies
                IndexesBuilt -> MaintainPolicies
          BlockHeader _ _ blockNo <-
            sieveBlock
              dbHandle
              progress
              redeemerCapture
              policyIndexing
              selectors
              (chainTipSlot serverTip)
              blockInMode
          let tip = fromChainTip serverTip
          -- On first catching the node's tip, build the deferred query indexes
          -- once: bulk catch-up ran index-free, and from here tip updates are
          -- small increments on the indexed tables.
          when (At blockNo >= tip) $ do
            st <- readIORef built
            case st of
              IndexesBuilt -> pure ()
              IndexesPending -> do
                -- Announce it. This is minutes of work on a large database and
                -- it happens with the heartbeat silenced (no blocks are being
                -- rolled forward while it runs), so without these two lines
                -- sieve looks hung at exactly the moment it finishes catching up.
                logLine
                  "reached tip — deriving the policy index and building query indexes (this can take a few minutes)"
                t0 <- getMonotonicTime
                buildIndexesOn dbHandle
                t1 <- getMonotonicTime
                logLine
                  ( "policy + query indexes built in "
                      <> duration (t1 - t0)
                      <> " — database now durable (WAL), following the tip"
                  )
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
boundedClient dbHandle progress redeemerCapture selectors since untilSlot =
  CSP.ChainSyncClientPipelined (clientIntersect <$> startPoints dbHandle since)
 where
  maxInFlight :: Word16
  maxInFlight = 50

  -- Resumes exactly as 'followingClient' does. A bounded run is the one most
  -- likely to be repeated with a larger --until, so re-reading the whole range
  -- each time is the most wasteful place to skip this.
  clientIntersect points =
    CSP.SendMsgFindIntersect points $
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \point serverTip -> do
            logLine ("resuming from " <> describePoint point)
            pure (clientIdle Syncing Origin (fromChainTip serverTip) Zero)
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
      Syncing ->
        case pipelineDecisionMax maxInFlight n clientTip serverTip of
          Collect -> case n of
            Succ predN -> collectFlushingWhenIdle dbHandle (clientNext Syncing predN)
          _ ->
            CSP.SendMsgRequestNextPipelined
              (pure ())
              (clientIdle Syncing clientTip serverTip (Succ n))

  clientNext
    :: BoundedPhase -> Nat n -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
  clientNext phase n =
    CSP.ClientStNext
      { CSP.recvMsgRollForward = \blockInMode serverTip ->
          case phase of
            Draining -> pure (clientIdle Draining Origin (fromChainTip serverTip) n)
            Syncing -> do
              -- Peek the slot BEFORE indexing: a block past the bound must not
              -- be written. --until is inclusive, so index
              -- iff slot <= untilSlot; the first block beyond the bound flips us
              -- to draining without being indexed. A block landing exactly on
              -- untilSlot is indexed and starts the drain in the same step.
              let BlockHeader slotNo _ blockNo =
                    case blockInMode of BlockInMode _ block -> getBlockHeader block
              if slotNo > untilSlot
                then pure (clientIdle Draining Origin (fromChainTip serverTip) n)
                else do
                  -- Always deferred: a bounded run never reaches the tip
                  -- transition, and --build-indexes derives the policy index
                  -- along with the rest.
                  _ <-
                    sieveBlock dbHandle progress redeemerCapture DeferPolicies selectors (Just untilSlot) blockInMode
                  let next = if slotNo >= untilSlot then Draining else Syncing
                  pure (clientIdle next (At blockNo) (fromChainTip serverTip) n)
      , CSP.recvMsgRollBackward = \point serverTip ->
          case phase of
            Draining -> pure (clientIdle Draining Origin (fromChainTip serverTip) n)
            Syncing -> do
              rollbackAbove dbHandle (chainPointSlot point)
              pure (clientIdle Syncing Origin (fromChainTip serverTip) n)
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

-- | The points to offer @MsgFindIntersect@ at startup.
--
-- An explicit @--since@ is taken literally — asking for a specific point and
-- silently getting a different one would be worse than failing.
--
-- Otherwise this is a resume: offer the stored checkpoints newest-first, then
-- genesis as the last resort. Before this existed the client always intersected
-- at genesis, so restarting an indexer without @--since@ re-read the entire
-- chain to rediscover data it already had.
--
-- Genesis is always appended, which is what makes 'recvMsgIntersectNotFound'
-- unreachable on the resume path: every chain contains it, so the node always
-- finds something.
startPoints :: DbHandle -> ChainPoint -> IO [ChainPoint]
startPoints dbHandle since = case since of
  ChainPoint{} -> pure [since]
  ChainPointAtGenesis -> do
    stored <- sampleCheckpoints (dbConn dbHandle)
    case (stored, traverse toChainPoint stored) of
      ([], _) -> pure [ChainPointAtGenesis]
      (newest : _, Just points) -> do
        logLine
          ( "resuming: offering "
              <> show (length points)
              <> " checkpoint(s), newest slot "
              <> commas (fst newest)
          )
        pure (points <> [ChainPointAtGenesis])
      -- A header hash the current build cannot read means the row is not one we
      -- wrote. Start over rather than guess at it.
      (_, Nothing) -> do
        logLine "checkpoints unreadable — starting from genesis"
        pure [ChainPointAtGenesis]
 where
  toChainPoint (slot, hash) =
    ChainPoint (SlotNo (fromIntegral slot))
      <$> either (const Nothing) Just (deserialiseFromRawBytes (proxyToAsType Proxy) hash)

-- | A chain point for the log line.
describePoint :: ChainPoint -> String
describePoint = \case
  ChainPointAtGenesis -> "genesis"
  ChainPoint slot _ -> "slot " <> commas (unSlotNo slot)
