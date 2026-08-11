{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Read query API (servant + warp) over the synced SQLite database.
--
-- The endpoints:
--
--   * @GET \/matches\/{pattern}@ — every dimension the indexer can match on
--   * @GET \/datums\/{hash}@ — a datum preimage, @{datum}@
--   * @GET \/scripts\/{hash}@ — a script preimage, @{script, language}@
--   * @GET \/checkpoints@ — a sample of stored chain points, newest first
--   * @GET \/checkpoints\/{slot-no}@ — the point at (or, by default, at-or-before)
--     a slot; @?strict@ demands the exact slot
--   * @GET \/patterns@ \/ @GET \/patterns\/{pattern}@ — the configured selectors,
--     all of them or those including a given pattern
--   * @PUT@\/@DELETE@ on @\/patterns@ — 501: reconfiguring a live indexer is a
--     deliberate non-feature; restart it with different @--select@s instead
--   * @DELETE \/matches\/{pattern}@ — prune everything a pattern matched,
--     refused while a configured selector still covers it
--   * @GET \/health@ — JSON health, kupo-shaped; @GET \/metrics@ — the same
--     facts in Prometheus exposition
--   * @GET \/metadata\/{slot-no}@ — a block's transaction metadata, fetched
--     from the node on demand (kupo stores none either); needs a node, so
--     serve-only mode refuses it
--
-- @\/matches@ covers every dimension. The pattern is parsed by
-- 'Cardano.Sieve.Selector.selectorFromText' — the same grammar @--select@ uses at
-- ingest — and 'planFor' maps the resulting 'Selector' onto the index that serves
-- it, so query syntax and ingest syntax cannot drift:
--
--   * @*@ — wildcard                      * @{payment}\/{delegation}@ — address parts
--   * a bech32\/base58\/base16 address     * @*\@{txid}@ — a whole transaction
--   * @{policy}.{name}@ \/ @{policy}.*@    * @{index}\@{txid}@ — one output
--
-- All thirteen of kupo\'s documented @\/matches@ parameters: @?unspent@, @?spent@,
-- @?resolve_hashes@, @?order@, @?created_after@, @?created_before@,
-- @?spent_after@, @?spent_before@, @?policy_id@, @?asset_name@,
-- @?transaction_id@, @?output_index@, plus the pattern itself. Passing neither
-- status flag returns both spent and unspent, as in kupo. Bare @\/matches@ with no
-- pattern is the wildcard.
--
-- The parameters are not all independent, and the invalid combinations are 400s
-- rather than silently-empty results: at most one lower and one upper slot bound
-- ('SlotBounds'), and @asset_name@ / @output_index@ each require their partner
-- ('Refinements').
--
-- Which table answers a request is decided by 'planFor': @?unspent@ reads the
-- indexed live set, anything spent-inclusive reads full history. Policy and asset
-- stay index-served either way because @policies@ is itself full-history.
--
-- Response shape is field-for-field identical to kupo's, verified by diffing
-- pinned rows of every kind (datum by hash, inline datum, no datum, spent,
-- reference script).
--
-- Known divergences from kupo (audited against its OpenAPI spec,
-- @kupo\/docs\/api\/nightly.yaml@): kupo streams every match in one unbounded
-- response, sieve serves 'pageLimit' rows per request with an @X-Next-Cursor@
-- header \/ @?after@ parameter to walk the rest — every match is reachable,
-- but a kupo client must learn to page. Within one slot the order is sieve's
-- total @(created_slot, rowid)@ rather than kupo's
-- @(created_slot, transaction_index, output_index)@, since matching that
-- exactly would cost the index-covered sort. A fresh read connection is
-- opened per request; a connection pool is a later refinement.
module Cardano.Server.Http
  ( runServer

    -- * Exposed for the test suite
  , Cursor (..)
  , cursorToText
  , cursorFromText
  )
where

import Cardano.Api
  ( AsType (AsAddressAny)
  , BlockHeader (BlockHeader)
  , BlockInMode (BlockInMode)
  , ChainPoint (ChainPoint, ChainPointAtGenesis)
  , ChainTip (ChainTip, ChainTipAtGenesis)
  , ConsensusModeParams (CardanoModeParams)
  , EpochSlots (EpochSlots)
  , Hash
  , LocalNodeConnectInfo (..)
  , NetworkId
  , SocketPath
  , Tx (ShelleyTx)
  , TxId
  , TxIn (TxIn)
  , TxIx (TxIx)
  , deserialiseFromRawBytes
  , deserialiseFromRawBytesHex
  , getBlockHeader
  , getBlockTxs
  , getLocalChainTip
  , getTxIdShelley
  , proxyToAsType
  , serialiseAddress
  , serialiseToRawBytes
  , shelleyBasedEraConstraints
  )
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.Core (TxAuxDataHash (unTxAuxDataHash), hashTxAuxData, originalBytes)
import Cardano.Ledger.Metadata (Metadatum (..))
import Cardano.Sieve.Node.FetchBlock (fetchBlockAtSlot)
import Cardano.Sieve.Node.Insert (busyTimeoutMs, sampleCheckpoints)
import Cardano.Sieve.Selector
  ( BootstrapFilter (IncludeBootstrap, OnlyShelley)
  , Selector (..)
  , credentialHashToBytes
  , includes
  , overlaps
  , selectorFromText
  )
import Cardano.Sieve.Value (decodeValue)
import Cardano.Slotting.Slot (SlotNo (SlotNo), unSlotNo)

import Control.Exception (SomeAsyncException (..), SomeException, fromException, throwIO, try)
import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null, String), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (showVersion)
import Data.Word (Word64, Word8)
import Database.SQLite.Simple
  ( Connection
  , Only (Only)
  , Query (Query)
  , SQLData (SQLBlob, SQLInteger)
  , execute
  , execute_
  , query
  , query_
  , withConnection
  , (:.) ((:.))
  )
import GHC.Clock (getMonotonicTime)
import Lens.Micro ((^.))
import Network.HTTP.Types.Status (status304)
import Network.Wai
  ( Middleware
  , mapResponseHeaders
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , requestMethod
  , responseLBS
  )
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (die)
import System.Posix.Files (fileExist)

import Paths_cardano_sieve qualified as Paths
import Servant
  ( Capture
  , CaptureAll
  , Delete
  , Get
  , Handler
  , Header
  , Headers
  , JSON
  , PlainText
  , Put
  , QueryFlag
  , QueryParam
  , Server
  , ServerError (errBody)
  , addHeader
  , err400
  , err501
  , err503
  , noHeader
  , serve
  , throwError
  , (:<|>) ((:<|>))
  , (:>)
  )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

-- | The query API.
type API =
  MatchesAPI :<|> CheckpointsAPI :<|> PatternsAPI :<|> HealthAPI :<|> PreimageAPI :<|> MetadataAPI

-- | On-demand transaction metadata, kupo's contract: never stored, asked of
-- the node per request. See 'metadataBySlot' for the mechanism and the edge
-- cases inherited deliberately.
type MetadataAPI =
  "metadata"
    :> Capture "slot-no" Int64
    :> QueryParam "transaction_id" Text
    :> Get '[JSON] (Headers '[Header "X-Block-Header-Hash" Text] [Value])

-- | Operational state, kupo's field names. @\/health@ answers JSON; @\/metrics@
-- answers the same facts in Prometheus exposition format (one divergence from
-- kupo, which content-negotiates both on either path).
type HealthAPI =
  "health" :> Get '[JSON] Value
    :<|> "metrics" :> Get '[PlainText] Text

-- | The configured selectors, read-only.
--
-- One 'CaptureAll' route per verb, because a pattern may span two path segments
-- (the @payment\/delegation@ form embeds a @\/@) and because it makes the bare
-- @\/patterns@ and @\/patterns\/{pattern}@ shapes one handler: no segments lists
-- everything, segments filter to the stored patterns that /include/ the given
-- one ('includes' — kupo's relation, so passing an address answers "which of my
-- selectors would match this?").
--
-- The write verbs exist to say no properly. kupo's PUT\/DELETE reconfigure a
-- RUNNING indexer — its handler rewinds the chain follower to re-index under
-- the new pattern set. Sieve's server deliberately has no indexer to rewind
-- (and when one shares the process, no channel to it), so these are 501 with
-- instructions, not 404: the resource exists, this server just will not mutate
-- it.
type PatternsAPI =
  "patterns" :> CaptureAll "pattern" Text :> Get '[JSON] [Text]
    :<|> "patterns" :> CaptureAll "pattern" Text :> Put '[JSON] Value
    :<|> "patterns" :> CaptureAll "pattern" Text :> Delete '[JSON] Value

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

-- | Preimage lookups by hash: the bodies behind the hashes a match reports.
--
-- Both return @null@ rather than a 404 for an unknown hash, as kupo does — a
-- referenced datum whose body has not been seen on chain is a normal state, not an
-- error.
type PreimageAPI =
  "datums" :> Capture "datum-hash" Text :> Get '[JSON] Value
    :<|> "scripts" :> Capture "script-hash" Text :> Get '[JSON] Value

type MatchesAPI =
  MatchesGetAPI
    :<|> "matches" :> CaptureAll "pattern" Text :> Delete '[JSON] Value

type MatchesGetAPI =
  "matches"
    :> CaptureAll "pattern" Text
    :> QueryFlag "unspent"
    :> QueryFlag "spent"
    :> QueryFlag "resolve_hashes"
    :> QueryParam "created_after" Int64
    :> QueryParam "created_before" Int64
    :> QueryParam "spent_after" Int64
    :> QueryParam "spent_before" Int64
    :> QueryParam "policy_id" Text
    :> QueryParam "asset_name" Text
    :> QueryParam "transaction_id" Text
    :> QueryParam "output_index" Word64
    :> QueryParam "order" Order
    :> QueryParam "after" Text
    :> Get '[JSON] (Headers '[Header "X-Next-Cursor" Text] [Value])

-- | Which way round results come back, from kupo's @?order@.
--
-- A real type rather than the raw 'Text' so servant parses and rejects it at the
-- boundary: @?order=sideways@ is a 400 before the handler runs, and the handler
-- cannot forget to validate it. Absent means 'MostRecentFirst', as in kupo.
data Order = MostRecentFirst | OldestFirst
  deriving (Eq, Show)

instance FromHttpApiData Order where
  parseUrlPiece = \case
    "most_recent_first" -> Right MostRecentFirst
    "oldest_first" -> Right OldestFirst
    other ->
      Left ("invalid order " <> other <> ": expected most_recent_first or oldest_first")

-- | The four slot-bound parameters, as they arrive.
--
-- Grouped rather than passed as four loose 'Maybe's because they are not
-- independent: kupo allows at most ONE lower bound and ONE upper bound, so
-- @created_after@ together with @spent_after@ is a contradiction, not a
-- conjunction. Keeping them in one value is what lets 'slotBoundsFor' state that
-- rule once.
data SlotBounds = SlotBounds
  { sbCreatedAfter :: Maybe Int64
  , sbCreatedBefore :: Maybe Int64
  , sbSpentAfter :: Maybe Int64
  , sbSpentBefore :: Maybe Int64
  }

-- | The post-filter parameters: they narrow whatever the pattern already selected
-- rather than choosing which index answers the query.
--
-- Grouped for the same reason as 'SlotBounds' — two of the four are only
-- meaningful in a pair. kupo's spec: @asset_name@ "can't be used alone and must be
-- provided alongside a @policy_id@", likewise @output_index@ with
-- @transaction_id@.
data Refinements = Refinements
  { rfPolicyId :: Maybe Text
  , rfAssetName :: Maybe Text
  , rfTransactionId :: Maybe Text
  , rfOutputIndex :: Maybe Word64
  }

-- | Whether @?resolve_hashes@ was asked for: does a match carry the datum and
-- script /bodies/ behind its hashes, or just the hashes?
--
-- Servant's 'QueryFlag' can only hand us a 'Bool', but it stops there. The flag
-- decides three separate things — which columns the SELECT projects, whether the
-- two preimage tables are joined, and what 'rowToJson' emits — and a bare 'Bool'
-- threaded to all three says nothing at any of them.
data HashResolution = ResolveHashes | LeaveHashes
  deriving (Eq, Show)

-- | The semi-join both the policy\/asset PATTERN and the @?policy_id@ post-filter
-- need: does a @policies@ row exist for this output under this policy (and, when
-- @extra@ adds it, this asset name)?
--
-- One definition for both callers so they cannot drift — they were previously two
-- copies of the same subquery, and only one of them had been written as a
-- semi-join. The policy hash is resolved to its @policy_ids@ surrogate by a scalar
-- subquery, keeping a single-value equality on the leading index column.
policiesExists :: Query -> Query
policiesExists extra =
  "EXISTS (SELECT 1 FROM policies p WHERE p.output_num = u.output_num \
  \AND p.policy_num = (SELECT policy_num FROM policy_ids WHERE policy_id = ?)"
    <> extra
    <> ")"

-- | Every extra @AND@ a request's filters contribute, with their parameters.
--
-- Returns 'Left' on a combination kupo rejects, so an impossible request fails
-- loudly instead of quietly matching nothing.
filtersFor :: Query -> SlotBounds -> Refinements -> Either Text [(Query, [SQLData])]
filtersFor createdCol bounds refine = do
  slots <- slotBoundsFor createdCol bounds
  refinements <- refinementsFor refine
  pure (slots <> refinements)

-- | At most one lower and one upper bound, each on whichever slot it names.
slotBoundsFor :: Query -> SlotBounds -> Either Text [(Query, [SQLData])]
slotBoundsFor
  createdCol
  SlotBounds
    { sbCreatedAfter = cAfter
    , sbCreatedBefore = cBefore
    , sbSpentAfter = sAfter
    , sbSpentBefore = sBefore
    } = do
    lower <- one "lower" "created_after" "spent_after" (">=") cAfter sAfter
    upper <- one "upper" "created_before" "spent_before" ("<=") cBefore sBefore
    pure (lower <> upper)
   where
    -- The created bound filters the creation slot, which for a policy/asset query is
    -- the copy on `policies` — the same column the ORDER BY uses, so the composite
    -- index still serves both. The spent bound filters the joined spends row.
    one which cName sName op c sp = case (c, sp) of
      (Just _, Just _) ->
        Left (cName <> " and " <> sName <> " are both " <> which <> " bounds; use one")
      (Just v, Nothing) -> Right [(createdCol <> " " <> op <> " ?", [SQLInteger v])]
      (Nothing, Just v) -> Right [("s.spent_slot " <> op <> " ?", [SQLInteger v])]
      (Nothing, Nothing) -> Right []

-- | The asset and output-reference post-filters.
refinementsFor :: Refinements -> Either Text [(Query, [SQLData])]
refinementsFor
  Refinements
    { rfPolicyId = pol
    , rfAssetName = asset
    , rfTransactionId = tx
    , rfOutputIndex = ix
    } = do
    assetFilter <- case (pol, asset) of
      (Nothing, Just _) -> Left "asset_name must be given alongside policy_id"
      (Nothing, Nothing) -> Right []
      (Just p, mName) -> do
        pid <- hex "policy_id" 28 p
        name <- traverse (hexAny "asset_name") mName
        Right
          [
            ( policiesExists (maybe "" (const " AND p.asset_name = ?") name)
            , SQLBlob pid : maybe [] ((: []) . SQLBlob) name
            )
          ]
    outputFilter <- case (tx, ix) of
      (Nothing, Just _) -> Left "output_index must be given alongside transaction_id"
      (Nothing, Nothing) -> Right []
      (Just t, mIx) -> do
        txid <- hex "transaction_id" 32 t
        Right $ case mIx of
          -- Both given identifies exactly one output, so compare the whole reference.
          Just i -> [("u.output_reference = ?", [SQLBlob (txid <> beWord64 i)])]
          Nothing -> [("u.transaction_id = ?", [SQLBlob txid])]
    pure (assetFilter <> outputFilter)
   where
    hex what n t = case Base16.decode (encodeUtf8 t) of
      Right b | BS.length b == n -> Right b
      Right b ->
        Left (what <> " must be " <> pack (show n) <> " bytes, got " <> pack (show (BS.length b)))
      Left _ -> Left (what <> " is not base16: " <> t)
    hexAny what t = case Base16.decode (encodeUtf8 t) of
      Right b -> Right b
      Left _ -> Left (what <> " is not base16: " <> t)

-- | Which side of the spend boundary a request wants.
--
-- The two @QueryFlag@s reach the handler as 'Bool's because a flag carries no
-- value for servant to parse — presence is the whole signal. So they are folded
-- into this the moment they arrive ('statusFromFlags') and the 'Bool's travel no
-- further; nothing downstream can transpose them.
data Status = OnlyUnspent | OnlySpent | AllMatches
  deriving (Eq, Show)

-- | Total mapping from the pair of flags. Neither flag means both sides, as in
-- kupo; both flags at once is a contradiction rather than a default.
statusFromFlags :: Bool -> Bool -> Either Text Status
statusFromFlags unspent spent = case (unspent, spent) of
  (True, True) -> Left "?spent and ?unspent are mutually exclusive"
  (True, False) -> Right OnlyUnspent
  (False, True) -> Right OnlySpent
  (False, False) -> Right AllMatches

-- | Serve the query API on @port@, reading from the SQLite database at @dbPath@.
-- Refuses to start unless the database is present and readable ('describeDatabase').
runServer :: Maybe (SocketPath, NetworkId) -> FilePath -> Int -> IO ()
runServer node dbPath port = do
  summary <- describeDatabase dbPath
  putStrLn
    ( "cardano-sieve query API: http://127.0.0.1:"
        <> show port
        <> "  (database "
        <> dbPath
        <> " — "
        <> summary
        <> ")"
    )
  Warp.run port (logRequests (cacheHeaders dbPath (serve (Proxy :: Proxy API) (server node dbPath))))

-- | Check the database before serving from it, and describe what is in it.
--
-- Worth doing loudly because the failure is otherwise silent: 'withConnection' is
-- opened per request and SQLite /creates/ a missing file on open, so a deleted or
-- mistyped @--database@ path used to start a server that logged nothing (no
-- requests yet), then answered every query with @[]@. Dying here, and printing the
-- row count and tip slot on success, makes "is this pointed at real data?"
-- answerable from the startup line alone.
describeDatabase :: FilePath -> IO String
describeDatabase dbPath = do
  exists <- fileExist dbPath
  unless exists $
    die (dbPath <> ": no such database — sync one first, or check --database")
  probed <-
    try (withReadConnection dbPath probe) :: IO (Either SomeException (Int, Maybe Int64, Bool))
  case probed of
    -- 'SomeException' also catches the 'AsyncCancelled' that 'race_' delivers
    -- when the sync thread dies first. That is not a database problem —
    -- blaming the file for it buried the real error once — so cancellation
    -- (and any other async exception) is rethrown, not reported.
    Left err
      | Just (SomeAsyncException _) <- fromException err -> throwIO err
      | otherwise ->
          die (dbPath <> ": not a readable cardano-sieve database — " <> show err)
    Right (rows, tip, indexed) -> do
      when (rows == 0) $
        putStrLn "WARNING: 0 unspent rows — every query will return []. Is the sync finished?"
      unless indexed $
        putStrLn "WARNING: query indexes absent — queries will be slow. Run with --build-indexes."
      pure
        ( show rows
            <> " unspent rows, tip slot "
            <> maybe "none" show tip
            <> if indexed then ", indexed" else ", NOT indexed"
        )
 where
  headOr :: a -> [Only a] -> a
  headOr d rs = case rs of Only x : _ -> x; [] -> d

  probe :: Connection -> IO (Int, Maybe Int64, Bool)
  probe conn = do
    -- Refuse a file left dirty by a crashed bulk sync before reporting anything
    -- from it — with journaling off there is no telling what state it is in, and
    -- serving confidently-wrong rows is worse than not starting.
    dirty <- query_ conn "PRAGMA user_version" :: IO [Only Int]
    case dirty of
      Only flag : _
        | flag /= 0 ->
            die
              ( dbPath
                  <> ": left dirty by an interrupted bulk sync — its contents cannot \
                     \be trusted. Delete it and sync again."
              )
      _ -> pure ()
    rows <- query_ conn "SELECT count(*) FROM unspent"
    tips <- query_ conn "SELECT max(created_slot) FROM unspent"
    -- One representative deferred index; they are all installed together.
    idxs <-
      query_
        conn
        "SELECT count(*) FROM sqlite_master WHERE type='index' AND name='unspentByAddress'"
    pure (headOr 0 rows, headOr Nothing tips, headOr (0 :: Int) idxs > 0)

-- | Open a read connection, matching the writer's lock-wait policy.
--
-- A plain 'withConnection' inherits SQLite's default @busy_timeout@ of 0 ms —
-- return @SQLITE_BUSY@ rather than wait. That is invisible while the server has
-- the database to itself, and breaks the moment an indexer shares the process
-- (@--serve@ alongside @--socket-path@): WAL keeps ordinary reads clear of the
-- writer, but the brief exclusive moments still collide and, with no timeout, the
-- reader errors instead of waiting a few milliseconds. Observed directly — a
-- fresh sync-and-serve failed with @ErrorBusy … database is locked@ on the
-- startup probe.
--
-- A connection is still opened per request; pooling is a separate refinement.
withReadConnection :: FilePath -> (Connection -> IO a) -> IO a
withReadConnection dbPath act =
  withConnection dbPath $ \conn -> do
    () <$ (query_ conn ("PRAGMA busy_timeout=" <> busyTimeoutMs) :: IO [Only Int])
    act conn

-- | Conditional-request support, kupo's contract exactly.
--
-- Every response carries @X-Most-Recent-Checkpoint@ (the newest indexed slot,
-- @0@ when the database is empty) and, when a checkpoint exists, @ETag@ — the
-- tip block's header hash as bare hex, no quotes. A request whose
-- @If-None-Match@ equals the current tag short-circuits to an empty @304@
-- before any handler runs: the chain has not moved since the client last
-- looked, so neither has any answer this server could give. That is what makes
-- polling cheap — kupo's spec documents the same @304@ on its read endpoints.
--
-- The tag is deliberately the raw hex kupo compares with plain equality, not an
-- RFC-quoted validator: kupo clients send back exactly what @ETag@ carried, and
-- matching kupo means matching that byte-for-byte.
--
-- One point lookup per request (the newest checkpoint, off the primary key).
-- kupo answers this from an in-memory health record instead; a cached tip is a
-- later refinement alongside the connection pool.
cacheHeaders :: FilePath -> Middleware
cacheHeaders dbPath app req respond = do
  tip <- withReadConnection dbPath $ \conn ->
    query_ conn "SELECT slot_no, header_hash FROM checkpoints ORDER BY slot_no DESC LIMIT 1"
      :: IO [(Int64, ByteString)]
  case tip of
    [] ->
      app req (respond . mapResponseHeaders (("X-Most-Recent-Checkpoint", "0") :))
    (slot, hash) : _ -> do
      let etag = encodeUtf8 (hexText hash)
          headers =
            [ ("X-Most-Recent-Checkpoint", B8.pack (show slot))
            , ("ETag", etag)
            ]
      if lookup "if-none-match" (requestHeaders req) == Just etag
        then respond (responseLBS status304 headers "")
        else app req (respond . mapResponseHeaders (headers <>))

-- | Minimal request log — "METHOD path?query  <ms>" per request — so it is
-- obvious the server is alive and requests are landing. The per-line cost is
-- negligible next to the SQL query and the HTTP round-trip.
logRequests :: Middleware
logRequests app req respond = do
  t0 <- getMonotonicTime
  app req $ \res -> do
    sent <- respond res
    t1 <- getMonotonicTime
    putStrLn $
      B8.unpack (requestMethod req)
        <> " "
        <> B8.unpack (rawPathInfo req)
        <> B8.unpack (rawQueryString req)
        <> "  "
        <> show (round ((t1 - t0) * 1000) :: Int)
        <> "ms"
    pure sent

server :: Maybe (SocketPath, NetworkId) -> FilePath -> Server API
server node dbPath =
  (matches :<|> matchesDelete dbPath)
    :<|> (checkpointsSample dbPath :<|> checkpointBySlot dbPath)
    :<|> (patternsGet dbPath :<|> patternsRefuse :<|> patternsRefuse)
    :<|> (healthJson node dbPath :<|> healthMetrics node dbPath)
    :<|> (datumByHash dbPath :<|> scriptByHash dbPath)
    :<|> metadataBySlot node dbPath
 where
  -- Servant delivers every parameter positionally and untyped — three bare
  -- 'Bool's and eight loose 'Maybe's in a row — so the boundary is where they get
  -- names. 'QueryFlag' can only give us 'Bool', but nothing past this line has to
  -- take one: the flags become 'HashResolution' and (in 'matchesByPattern') a
  -- 'Status', and the filters become the two records.
  matches segs unspent spent resolve cAfter cBefore sAfter sBefore pol asset tx ix =
    matchesByPattern
      dbPath
      segs
      unspent
      spent
      (if resolve then ResolveHashes else LeaveHashes)
      (SlotBounds cAfter cBefore sAfter sBefore)
      (Refinements pol asset tx ix)

-- (?order and ?after stay positional through here; matchesByPattern
-- reconciles them, since only the combination is meaningful.)

-- | Reject a request with the reason in the body. Top-level because both the
-- \/matches and \/patterns handlers validate client-supplied patterns.
badRequest :: Text -> Handler a
badRequest msg = throwError err400{errBody = LBS.fromStrict (encodeUtf8 msg)}

-- | @DELETE \/matches\/{pattern}@ — prune everything the pattern matched:
-- the outputs, their live-set rows, their spend records and their policy index
-- rows, in one transaction, answering @{"deleted": n}@ with the count of
-- outputs removed. This is how disk is reclaimed after narrowing interest —
-- e.g. having indexed under @*@ and later caring about one address — without
-- a wipe-and-resync.
--
-- Refused ('overlaps') while a configured selector still covers the pattern:
-- the indexer would re-create the rows from the next block, so the delete
-- would be pointless churn. kupo guards identically. Remove the selector from
-- @--select@ (and restart) first.
--
-- Row selection reuses 'planFor' — the same planner every @\/matches@ query
-- goes through — with 'AllMatches', so what a pattern DELETES is exactly what
-- it would have returned. The doomed set is materialised once into a TEMP
-- table (per-connection, so concurrent deletes cannot collide) and the four
-- tables are pruned from it, @policies@ before @outputs@ for the foreign key,
-- as 'rollbackAbove' does.
--
-- A policy or asset pattern reads the @policies@ index to find its rows, and
-- that index may legitimately not exist yet (it is derived at the tip). On an
-- underived database the delete would silently remove nothing, so that case
-- is refused with instructions rather than allowed to lie.
matchesDelete :: FilePath -> [Text] -> Handler Value
matchesDelete dbPath segs = do
  sel <- case selectorFromText (T.intercalate "/" segs) of
    Left err -> badRequest ("invalid pattern: " <> pack (show err))
    Right s -> pure s
  active <- liftIO $ withReadConnection dbPath $ \conn ->
    query_ conn "SELECT selector FROM patterns"
  let parse t = either (\e -> error ("stored selector unparseable: " <> show e)) id (selectorFromText t)
  when (sel `overlaps` [parse t | Only t <- active]) $
    badRequest
      "pattern overlaps a configured selector: the indexer would re-create \
      \these matches from the next block. Remove it from --select (and \
      \restart the indexer) before deleting its matches."
  plan <- either badRequest pure (planFor AllMatches sel)
  needsPolicyIndex <- case sel of
    SelectPolicyId{} -> pure True
    SelectAssetId{} -> pure True
    _ -> pure False
  underived <- liftIO $ withReadConnection dbPath $ \conn -> do
    hasPolicies <- query_ conn "SELECT EXISTS (SELECT 1 FROM policies)" :: IO [Only Int]
    hasOutputs <- query_ conn "SELECT EXISTS (SELECT 1 FROM outputs)" :: IO [Only Int]
    pure (needsPolicyIndex && hasPolicies == [Only 0] && hasOutputs == [Only 1])
  when underived $
    badRequest
      "the policy index has not been derived yet, so this pattern would match \
      \nothing to delete. Run --build-indexes first."
  deleted <- liftIO $ withReadConnection dbPath $ \conn -> do
    execute_ conn "BEGIN TRANSACTION"
    execute
      conn
      ( "CREATE TEMP TABLE doomed AS SELECT u.output_num AS output_num FROM "
          <> planFrom plan
          <> " WHERE "
          <> planWhere plan
      )
      (planParams plan)
    counted <- query_ conn "SELECT count(*) FROM doomed" :: IO [Only Int]
    execute_ conn "DELETE FROM policies WHERE output_num IN (SELECT output_num FROM doomed)"
    execute_ conn "DELETE FROM spends WHERE output_num IN (SELECT output_num FROM doomed)"
    execute_ conn "DELETE FROM unspent WHERE output_num IN (SELECT output_num FROM doomed)"
    execute_ conn "DELETE FROM outputs WHERE output_num IN (SELECT output_num FROM doomed)"
    execute_ conn "DROP TABLE doomed"
    execute_ conn "COMMIT"
    pure (case counted of Only n : _ -> n; [] -> 0)
  pure (object ["deleted" .= deleted])

-- | @GET \/patterns@ and @GET \/patterns\/{pattern}@ — the selectors this
-- database is indexed under, as stored (the canonical text 'reconcileSelectors'
-- wrote). No segments lists all of them; a pattern filters to those that
-- include it, and a malformed pattern is a 400.
patternsGet :: FilePath -> [Text] -> Handler [Text]
patternsGet dbPath segs = do
  stored <- liftIO $ withReadConnection dbPath $ \conn ->
    query_ conn "SELECT selector FROM patterns ORDER BY selector"
  let texts = [t | Only t <- stored]
  case segs of
    [] -> pure texts
    _ -> do
      needle <- case selectorFromText (T.intercalate "/" segs) of
        Left err -> badRequest ("invalid pattern: " <> pack (show err))
        Right sel -> pure sel
      -- Stored rows are canonical text written by selectorToText, so a parse
      -- failure here is corruption, not client error — let it 500 loudly.
      let parse t = either (\e -> error ("stored selector unparseable: " <> show e)) id (selectorFromText t)
      pure [t | t <- texts, parse t `includes` needle]

-- | @PUT@ and @DELETE@ under @\/patterns@: refused, with the reason and the
-- alternative in the body.
patternsRefuse :: [Text] -> Handler Value
patternsRefuse _ =
  throwError
    err501
      { errBody =
          "sieve does not reconfigure a live indexer: adding or removing \
          \patterns mid-sync leaves the database incomplete for what it claims \
          \to index (kupo re-syncs from a rollback point instead). Stop the \
          \indexer and restart it with the --select set you want; it will \
          \refuse mismatches and tell you what it was built with."
      }

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

-- | The language a stored script's discriminator byte names. Values confirmed
-- against both kupo's table and its OpenAPI enum.
scriptLanguage :: Word8 -> Value
scriptLanguage = \case
  0 -> "native"
  1 -> "plutus:v1"
  2 -> "plutus:v2"
  3 -> "plutus:v3"
  n -> String ("unknown:" <> pack (show n))

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

-- | @GET \/metadata\/{slot-no}@ — every transaction's metadata in the block at
-- a slot, fetched from the node on demand ('fetchBlockAtSlot'); kupo's contract,
-- including never storing any of it. The ancestor to walk from is the
-- checkpoint at-or-before @slot − 1@ — the same row @\/checkpoints\/{slot}@
-- serves — or genesis on an empty range.
--
-- Edge cases match kupo deliberately:
--
--   * slot @0@ is a hardcoded @[]@ with no header: nothing can have an
--     ancestor there. A negative slot is a 400.
--   * an unrecognised ancestor, or a rollback racing the walk, is a 400
--     (kupo's \"no ancestor\" answer) — the client should retry.
--   * a slot nobody minted in answers with the NEXT block's metadata: the
--     fetch stops at the first block at-or-past the target, checking nothing
--     (kupo takes the single block after its intersection, same thing). The
--     @X-Block-Header-Hash@ header carries the hash the answer actually came
--     from, and kupo's spec pushes verifying it onto the client.
--   * @?transaction_id@ filters to one transaction's items; a malformed id is
--     a 400.
--
-- One divergence: serve-only mode (no @--socket-path@) has no node to ask, so
-- it refuses with a 503 rather than pretending — the same honesty
-- @\/health@'s @connection_status@ shows in that mode.
metadataBySlot
  :: Maybe (SocketPath, NetworkId)
  -> FilePath
  -> Int64
  -> Maybe Text
  -> Handler (Headers '[Header "X-Block-Header-Hash" Text] [Value])
metadataBySlot node dbPath slot txIdParam = do
  (socket, network) <- case node of
    Nothing ->
      throwError
        err503
          { errBody =
              "metadata is fetched from the node on demand, never stored (kupo \
              \does the same) — and this server has no node: it is serving an \
              \already-synced database. Run --serve alongside --socket-path to \
              \serve /metadata."
          }
    Just sn -> pure sn
  wanted <- case txIdParam of
    Nothing -> pure Nothing
    Just t -> case deserialiseFromRawBytesHex @TxId (encodeUtf8 t) of
      Left _ -> badRequest "invalid transaction_id: expected a base16-encoded transaction id"
      Right txid -> pure (Just txid)
  when (slot < 0) $
    badRequest "slot-no must be a non-negative slot number"
  if slot == 0
    then pure (noHeader [])
    else do
      ancestorRow <- liftIO $ withReadConnection dbPath $ \conn ->
        query
          conn
          "SELECT slot_no, header_hash FROM checkpoints \
          \WHERE slot_no <= ? ORDER BY slot_no DESC LIMIT 1"
          (Only (slot - 1))
          :: IO [(Int64, ByteString)]
      ancestor <- case ancestorRow of
        [] -> pure ChainPointAtGenesis
        (aslot, hash) : _ ->
          -- Stored by the indexer from a decoded header, so a parse failure
          -- here is corruption, not client error — let it 500 loudly.
          case deserialiseFromRawBytes (proxyToAsType (Proxy @(Hash BlockHeader))) hash of
            Left err -> error ("stored header hash unparseable at slot " <> show aslot <> ": " <> show err)
            Right h -> pure (ChainPoint (SlotNo (fromIntegral aslot)) h)
      fetched <-
        liftIO $ try (fetchBlockAtSlot socket network ancestor (SlotNo (fromIntegral slot)))
      case fetched of
        Left err
          | Just (SomeAsyncException _) <- fromException (err :: SomeException) ->
              liftIO (throwIO err)
          | otherwise ->
              throwError
                err503
                  { errBody =
                      "the node did not answer: "
                        <> LBS.fromStrict (encodeUtf8 (pack (show err)))
                  }
        Right Nothing ->
          badRequest
            "no known ancestor to that slot — a rollback likely raced this \
            \request; retry it"
        Right (Just (BlockInMode _ blk)) -> do
          let BlockHeader _ headerHash _ = getBlockHeader blk
          pure $
            addHeader
              (hexText (serialiseToRawBytes headerHash))
              (metadataItems wanted (getBlockTxs blk))

-- | One item per transaction carrying auxiliary data, in block order — kupo's
-- shape: @{hash, raw, schema}@. Byron transactions cannot carry metadata (and
-- 'getBlockTxs' yields none for Byron blocks), so a Byron block is @[]@, as in
-- kupo.
--
-- @raw@ is the auxiliary data's on-chain serialisation and @hash@ its
-- blake2b-256 ('hashTxAuxData') — the hash the transaction body committed to.
-- kupo instead re-encodes the aux data into its newest era's format and
-- recomputes the hash over the re-encoding. The two agree wherever the
-- on-chain bytes already use the Alonzo tag-259 format — measured at 96% of
-- metadata-carrying preview blocks in slots 0–4M (2,443 of 2,545 sampled) —
-- and differ where a transaction shipped the legacy Shelley (bare map) or
-- Allegra (@[metadata, scripts]@ array) encoding, which stays legal in
-- Alonzo-era-and-later blocks: kupo then reports bytes that are not on the
-- chain and a hash the transaction body does not carry, while sieve's pair
-- round-trips against the chain. Diverging from kupo here is deliberate,
-- the same ruling as the spend-redeemer index: match the ledger, not kupo's
-- bug. Both stay self-consistent (@hash == blake2b-256(raw)@ either way).
-- @schema@ — which never differs — mirrors kupo's @encodeMetadatum@
-- constructor for constructor.
metadataItems :: Maybe TxId -> [Tx era] -> [Value]
metadataItems wanted = mapMaybe $ \(ShelleyTx sbe ledgerTx) ->
  shelleyBasedEraConstraints sbe $ do
    aux <- L.strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL)
    guard (maybe True (== getTxIdShelley sbe (ledgerTx ^. L.bodyTxL)) wanted)
    pure $
      object
        [ "hash" .= hexText (L.hashToBytes (L.extractHash (unTxAuxDataHash (hashTxAuxData aux))))
        , "raw" .= hexText (originalBytes aux)
        , "schema"
            .= object
              [ Key.fromString (show label) .= metadatumJson m
              | (label, m) <- Map.toAscList (aux ^. L.metadataTxAuxDataL)
              ]
        ]

-- | kupo's @schema@ rendering of one metadatum — its @encodeMetadatum@, shape
-- for shape: five primitives, each wrapped in a one-field object naming it.
metadatumJson :: Metadatum -> Value
metadatumJson = \case
  I n -> object ["int" .= n]
  S txt -> object ["string" .= txt]
  B bytes -> object ["bytes" .= hexText bytes]
  List xs -> object ["list" .= map metadatumJson xs]
  Map kvs ->
    object
      [ "map"
          .= [ object ["k" .= metadatumJson k, "v" .= metadatumJson v]
             | (k, v) <- kvs
             ]
      ]

-- | How many matches one request returns. Kupo streams every match; we page, so
-- a hot key cannot turn one request into a multi-hundred-megabyte response.
-- A full page carries an @X-Next-Cursor@ header; @?after@ with its value
-- resumes exactly where the page stopped, so every match is reachable — the
-- remaining divergence from kupo is that a client must walk pages rather than
-- read one unbounded body.
pageLimit :: Int
pageLimit = 100

-- | Where a page stopped: the sort key of its last row — @(created_slot,
-- rowid)@ of the base table — plus the direction it was walking. The next page
-- is everything strictly past it, which needs no OFFSET (O(page) per page, not
-- O(pages walked)) and no server-side state.
--
-- The rowid is the tiebreak that makes the sort total, and it is deliberately
-- @u.rowid@ rather than @output_num@: within duplicate @(column, created_slot)@
-- keys SQLite orders index entries by rowid, so the existing composite indexes
-- serve the two-column @ORDER BY@ with no sort pass and no index change.
-- (On @outputs@ the rowid IS @output_num@; on @unspent@ it is the hidden one.)
--
-- Consequences a client can observe, both accepted: a cursor is only
-- meaningful for the same query it came from (the base table, and so the
-- rowid, changes with @?unspent@\/@?spent@); and a rollback that re-inserts
-- rows mid-walk can shift them relative to a held cursor — pagination under a
-- reorg is best-effort, where kupo's single-transaction stream is a snapshot.
data Cursor = Cursor
  { cursorDesc :: Bool
  , cursorSlot :: Int64
  , cursorRowId :: Int64
  }
  deriving (Eq, Show)

-- | The wire form is opaque on purpose: 17 base16 bytes (direction, slot,
-- rowid, the integers big-endian), promising clients nothing they could
-- usefully parse — the contract is \"hand back what @X-Next-Cursor@ gave you\".
cursorToText :: Cursor -> Text
cursorToText (Cursor desc slot rowid) =
  hexText
    ( BS.singleton (if desc then 1 else 0)
        <> beWord64 (fromIntegral slot)
        <> beWord64 (fromIntegral rowid)
    )

cursorFromText :: Text -> Maybe Cursor
cursorFromText t = case Base16.decode (encodeUtf8 t) of
  Right raw
    | BS.length raw == 17
    , Just desc <- case BS.head raw of
        0 -> Just False
        1 -> Just True
        _ -> Nothing ->
        Just (Cursor desc (word64At 1 raw) (word64At 9 raw))
  _ -> Nothing
 where
  word64At off = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 . BS.take 8 . BS.drop off

-- | @GET \/matches\/{pattern}@ — the whole query surface, dispatched on the parsed
-- 'Selector' so every dimension the indexer can match is also queryable, over
-- either side of the spend boundary.
--
-- The pattern grammar is 'selectorFromText', the same one @--select@ uses at
-- ingest, so anything you can index by you can query by with identical syntax.
matchesByPattern
  :: FilePath
  -> [Text]
  -> Bool
  -- ^ @?unspent@, as servant's 'QueryFlag' delivers it; paired with the next into
  -- a 'Status' below, since only the /combination/ is meaningful.
  -> Bool
  -- ^ @?spent@.
  -> HashResolution
  -> SlotBounds
  -> Refinements
  -> Maybe Order
  -> Maybe Text
  -- ^ @?after@ — an 'X-Next-Cursor' value from a previous page.
  -> Handler (Headers '[Header "X-Next-Cursor" Text] [Value])
matchesByPattern dbPath segments unspentFlag spentFlag resolveHashes bounds refine order afterParam = do
  -- The pattern is captured as PATH SEGMENTS and rejoined, because the
  -- payment/delegation form embeds a '/' and so spans two segments. kupo does the
  -- same (it matches on @"matches" : args@). No segments at all is the bare
  -- /matches route, which kupo treats as the wildcard.
  let pat = if null segments then "*" else T.intercalate "/" segments
  status <- either badRequest pure (statusFromFlags unspentFlag spentFlag)
  selector <- case selectorFromText pat of
    Left err -> badRequest ("invalid pattern: " <> pack (show err))
    Right s -> pure s
  plan <- either badRequest pure (planFor status selector)
  filters <- either badRequest pure (filtersFor (planSlotCol plan) bounds refine)
  cursor <- case afterParam of
    Nothing -> pure Nothing
    Just t -> case cursorFromText t of
      Nothing -> badRequest "invalid ?after: pass back exactly what X-Next-Cursor carried"
      Just c -> pure (Just c)
  -- A cursor was cut under one direction; walking it the other way would skip
  -- everything between the cursor and the far end. ?order may restate the
  -- cursor's direction but not contradict it.
  desc <- case cursor of
    Nothing -> pure (fromMaybe MostRecentFirst order == MostRecentFirst)
    Just c -> case order of
      Just o
        | (o == MostRecentFirst) /= cursorDesc c ->
            badRequest "?after cursor was issued under the opposite ?order"
      _ -> pure (cursorDesc c)
  rows <- liftIO $ withReadConnection dbPath $ \conn ->
    query
      conn
      (planSql plan filters desc cursor)
      ( planParams plan
          <> concatMap snd filters
          <> foldMap (\c -> [SQLInteger (cursorSlot c), SQLInteger (cursorRowId c)]) cursor
      )
  -- One row beyond the page answers "is there more?" without a second query;
  -- it is dropped, and its presence is what puts X-Next-Cursor on the response.
  let (page, overflow) = splitAt pageLimit rows
      withNext = case (overflow, reverse page) of
        (_ : _, (core :. Only rowid) : _) ->
          addHeader (cursorToText (Cursor desc (rowSlot core) rowid))
        _ -> noHeader
  pure (withNext (map (\(core :. _rowid) -> rowToJson resolveHashes core) page))
 where
  rowSlot ((_, _, _, _, _, _, _, slot, _) :. _) = slot :: Int64
  -- One row shape for every status, so a single 'rowToJson' serves them all. The
  -- spends join is a primary-key probe that yields nothing on the @unspent@ base
  -- (those rows are deleted when spent), which at 'pageLimit' rows is immaterial.
  planSql plan filters desc cursor =
    "SELECT u.output_reference, u.transaction_index, u.address, u.value, u.datum_hash, \
    \u.datum_type, u.reference_script_hash, u.created_slot, bc.header_hash, \
    \s.spent_slot, bs.header_hash, s.spending_transaction_id, \
    \s.spending_input_index, s.redeemer, "
      <> (case resolveHashes of ResolveHashes -> "bd.datum, sc.script"; LeaveHashes -> "NULL, NULL")
      -- The rowid rides along for the cursor and is stripped before rendering.
      <> ", u.rowid FROM "
      <> planFrom plan
      <> " LEFT JOIN blocks bc ON bc.slot_no = u.created_slot \
         \LEFT JOIN spends s ON s.output_num = u.output_num \
         \LEFT JOIN blocks bs ON bs.slot_no = s.spent_slot"
      -- Only joined when asked for: both are primary-key probes, but resolving on
      -- every match would ship a datum body per row (and one popular datum is
      -- referenced by 2,480 outputs, so a large page would repeat it).
      <> ( case resolveHashes of
             ResolveHashes ->
               " LEFT JOIN binary_data bd ON bd.datum_hash = u.datum_hash \
               \LEFT JOIN scripts sc ON sc.script_hash = u.reference_script_hash"
             LeaveHashes -> ""
         )
      <> " WHERE "
      <> planWhere plan
      <> foldMap (\(cond, _) -> " AND " <> cond) filters
      -- The continuation: strictly past the cursor in the walk's direction. A
      -- row value, so the comparison follows the same two-column order the
      -- ORDER BY names and the index serves.
      <> ( case cursor of
             Just _ -> " AND (u.created_slot, u.rowid) " <> (if desc then "<" else ">") <> " (?, ?)"
             Nothing -> ""
         )
      <> " ORDER BY "
      <> planSlotCol plan
      <> (if desc then " DESC" else " ASC")
      -- The rowid tiebreak makes the order total (a cursor needs an exact
      -- resume point) and is free: within duplicate index keys SQLite already
      -- stores entries in rowid order, so the composite indexes cover this
      -- two-column sort exactly as they covered the one-column one.
      <> ", u.rowid"
      <> (if desc then " DESC" else " ASC")
      <> " LIMIT "
      <> Query (pack (show (pageLimit + 1)))

-- | The table expression, filter and parameters for one selector.
--
-- 'planSlotCol' is which table's @created_slot@ to filter and sort on, and it
-- matters: for the policy/asset dimensions it must be the @policies@ copy, so the
-- composite @(policy_num, …, created_slot)@ index serves the @ORDER BY@ from the
-- same index it filtered with. Sorting on @u.created_slot@ instead would reduce
-- those to a gather-and-sort of every match.
data Plan = Plan
  { planFrom :: Query
  , planWhere :: Query
  , planSlotCol :: Query
  , planParams :: [SQLData]
  }

-- | Map a selector and status onto a query plan, or say why there is none.
--
-- The base table is chosen by status and always aliased @u@: @unspent@ (the
-- indexed live set) for @?unspent@, @outputs@ (full history) otherwise. That
-- substitution works because @outputs@ carries the same columns, which is what the
-- credential columns on it are for.
--
-- Cost is uneven by design and worth knowing before reading a benchmark:
-- @?unspent@ is index-served on every dimension; policy and asset stay
-- index-served for any status, because @policies@ is itself full-history and
-- indexed; the remaining dimensions over full history are scans of @outputs@,
-- which is primary-key-only on purpose.
planFor :: Status -> Selector -> Either Text Plan
planFor status = \case
  -- The two wildcards differ: bare @*@ is everything, @*\/*@ excludes Byron.
  -- Byron rows are exactly those with no payment credential (the decode stage
  -- stores NULL for bootstrap addresses, by construction), so the column IS the
  -- bootstrap filter. Discarding the filter here once made @*\/*@ return — and,
  -- via DELETE \/matches, would have deleted — Byron outputs.
  SelectAll IncludeBootstrap -> Right (onBase "1" [])
  SelectAll OnlyShelley -> Right (onBase "u.payment_credential IS NOT NULL" [])
  SelectExact addr ->
    Right (onBase "u.address = ?" [blob (serialiseToRawBytes addr)])
  SelectPayment ch ->
    Right (onBase "u.payment_credential = ?" [blob (credentialHashToBytes ch)])
  SelectDelegation ch ->
    Right (onBase "u.delegation_credential = ?" [blob (credentialHashToBytes ch)])
  SelectPaymentAndDelegation p d ->
    Right
      ( onBase
          "u.payment_credential = ? AND u.delegation_credential = ?"
          [blob (credentialHashToBytes p), blob (credentialHashToBytes d)]
      )
  SelectTransactionId txid ->
    Right (onBase "u.transaction_id = ?" [blob (serialiseToRawBytes txid)])
  SelectOutputReference txin ->
    Right (onBase "u.output_reference = ?" [blob (encodeOutputRef txin)])
  SelectPolicyId pid ->
    Right (viaPolicies "" [blob (serialiseToRawBytes pid)])
  SelectAssetId pid name ->
    Right
      ( viaPolicies
          " AND p.asset_name = ?"
          [blob (serialiseToRawBytes pid), blob (serialiseToRawBytes name)]
      )
  SelectMetadataTag _ ->
    Left
      "metadata tags are an ingest-only dimension: they decide what to index and \
      \are not recorded per output, so they cannot be queried after the fact"
 where
  blob = SQLBlob

  baseTable = case status of
    OnlyUnspent -> "unspent u"
    OnlySpent -> "outputs u"
    AllMatches -> "outputs u"

  -- ?spent restricts to rows that have a spends entry. ?unspent needs no such
  -- filter: the live-set table only ever holds unspent outputs.
  statusFilter = case status of
    OnlyUnspent -> ""
    OnlySpent -> " AND s.output_num IS NOT NULL"
    AllMatches -> ""

  onBase cond params = Plan baseTable (cond <> statusFilter) "u.created_slot" params

  -- EXISTS, never a JOIN. @policies@ holds one row per (output, policy, asset), so
  -- joining it emits an output once per MATCHING ASSET: an output holding six
  -- assets of a policy came back six times, 14,780 rows where kupo returns 2,464.
  -- A semi-join asks only whether such a row exists.
  --
  -- Measured on the hottest policy at 4M, all three candidates return the correct
  -- 2,464, but EXISTS yields its FIRST row in 0.00 s with no auxiliary structure;
  -- GROUP BY needs 0.70 s because it must materialise every row before emitting
  -- one; DISTINCT accumulates a seen-set b-tree that grows with the result.
  --
  -- The cost is that the sort moves to the base table's @created_slot@. On the
  -- @?unspent@ path @unspentByCreatedSlot@ covers it, so there is still no sort. On
  -- a spent-inclusive query the base is @outputs@, which is primary-key-only by
  -- design, so that one does sort — consistent with historical queries there being
  -- best-effort scans anyway.
  viaPolicies extra params =
    Plan
      baseTable
      (policiesExists extra <> statusFilter)
      "u.created_slot"
      params

-- | Encode an output reference the way the schema stores it: 32 transaction-id
-- bytes then the output index as a big-endian 'Word64'. Must stay in step with
-- @Cardano.Sieve.Node.Decode.encodeOutputRef@.
encodeOutputRef :: TxIn -> ByteString
encodeOutputRef (TxIn txid (TxIx ix)) =
  serialiseToRawBytes txid <> beWord64 (fromIntegral ix)

-- | An output index as the schema stores it: big-endian, fixed width, so byte
-- order matches numeric order and a reference can be compared or ranged as bytes.
beWord64 :: Word64 -> ByteString
beWord64 = LBS.toStrict . toLazyByteString . word64BE

-- | One row as JSON, mirroring kupo's match shape field for field.
rowToJson
  :: HashResolution
  -> ( ByteString
     , Int64
     , ByteString
     , ByteString
     , Maybe ByteString
     , Maybe Int64
     , Maybe ByteString
     , Int64
     , Maybe ByteString
     )
    :. ( Maybe Int64
       , Maybe ByteString
       , Maybe ByteString
       , Maybe Int64
       , Maybe ByteString
       , Maybe ByteString
       , Maybe ByteString
       )
  -> Value
rowToJson
  resolved
  ( (oref, txIx, addr, val, mDatum, mDatumType, mScript, slot, mHeader)
      :. (mSpentSlot, mSpentHeader, mSpendTx, mInputIx, mRedeemer, mDatumBody, mScriptBody)
    ) =
    object
      ( [ "transaction_id" .= hexText (BS.take 32 oref)
        , "transaction_index" .= txIx
        , "output_index" .= outputIndex oref
        , "address" .= addressText addr
        , "value" .= kupoValue val
        , "datum_hash" .= (hexText <$> mDatum)
        , "script_hash" .= (hexText <$> mScript)
        , "created_at" .= object ["slot_no" .= slot, "header_hash" .= (hexText <$> mHeader)]
        , "spent_at" .= spentAt
        ]
          -- Present only when there IS a datum: kupo omits the key entirely
          -- rather than emitting null, and a null would read as "no datum" to a
          -- client that checks for the field's presence.
          <> ["datum_type" .= t | Just t <- [datumTypeText =<< mDatumType]]
          -- ?resolve_hashes inlines the bodies. Emitted whenever resolving was
          -- asked for, null when the body is not stored, so a client can tell
          -- "not resolved" from "resolved, nothing there".
          <> [ "datum" .= (hexText <$> mDatumBody) | resolved == ResolveHashes
             ]
          <> [ "script" .= (scriptBodyJson =<< mScriptBody) | resolved == ResolveHashes
             ]
      )
   where
    -- Present only for a spent output. 'redeemer' is always null for now: the
    -- column exists but the write path does not populate it yet.
    spentAt = case mSpentSlot of
      Nothing -> Null
      Just spentSlot ->
        object
          [ "slot_no" .= spentSlot
          , "header_hash" .= (hexText <$> mSpentHeader)
          , "transaction_id" .= (hexText <$> mSpendTx)
          , "input_index" .= mInputIx
          , "redeemer" .= (hexText <$> mRedeemer)
          ]

-- | A stored script blob as @{script, language}@, splitting off the leading
-- discriminator byte — the same shape 'scriptByHash' returns.
scriptBodyJson :: ByteString -> Maybe Value
scriptBodyJson body = case BS.uncons body of
  Nothing -> Nothing
  Just (tag, raw) -> Just (object ["script" .= hexText raw, "language" .= scriptLanguage tag])

-- | Render the stored datum-type flag the way kupo does. Anything other than the
-- two known encodings yields 'Nothing' rather than a guess, so a future third
-- kind cannot be silently mislabelled as one of these.
datumTypeText :: Int64 -> Maybe Text
datumTypeText = \case
  0 -> Just "hash"
  1 -> Just "inline"
  _ -> Nothing

-- | Base16 of raw bytes, as text.
hexText :: ByteString -> Text
hexText = decodeUtf8 . Base16.encode

-- | The output reference is 32 tx-id bytes then a big-endian Word64 output
-- index; recover the index.
outputIndex :: ByteString -> Word64
outputIndex = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 . BS.drop 32

-- | Render the raw address bytes as kupo does (bech32 for Shelley, base58 for
-- Byron); fall back to hex if it does not decode.
addressText :: ByteString -> Text
addressText raw = case deserialiseFromRawBytes AsAddressAny raw of
  Right a -> serialiseAddress a
  Left _ -> hexText raw

-- | Reshape a stored value into kupo's @{coins, assets:{"policy.name":qty}}@,
-- quantities rendered as strings.
--
-- The stored form is the compact CBOR of "Cardano.Sieve.Value"; an asset with an
-- empty name renders as just the bare policy id, as kupo does. A value that fails
-- to decode yields zero coins and no assets rather than failing the request —
-- a single unreadable row should not take out a whole page of results.
kupoValue :: ByteString -> Value
kupoValue raw = case decodeValue raw of
  Left _ -> object ["coins" .= ("0" :: Text), "assets" .= object []]
  Right (coins, assets) ->
    object
      [ "coins" .= pack (show coins)
      , "assets"
          .= object
            [ Key.fromText (assetKey pid name) .= pack (show q)
            | (pid, name, q) <- assets
            ]
      ]
 where
  assetKey pid name
    | BS.null name = hexText pid
    | otherwise = hexText pid <> "." <> hexText name
