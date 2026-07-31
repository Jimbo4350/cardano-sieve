{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Read query API (servant + warp) over the synced SQLite database.
--
-- Three endpoints:
--
--   * @GET \/matches\/{pattern}@ — every dimension the indexer can match on
--   * @GET \/datums\/{hash}@ — a datum preimage, @{datum}@
--   * @GET \/scripts\/{hash}@ — a script preimage, @{script, language}@
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
-- Known gaps against kupo (audited against its OpenAPI spec,
-- @kupo\/docs\/api\/nightly.yaml@): the @\/patterns@, @\/checkpoints@, @\/metadata@,
-- @\/health@ and @\/metrics@ endpoints are absent, as is @DELETE
-- \/matches\/{pattern}@; results are capped at 'pageLimit' where kupo streams every
-- match; and within one slot the result order differs, since
-- matching kupo\'s @(created_slot, transaction_index, output_index)@ exactly would
-- cost the index-covered sort. A fresh read connection is opened per request; a
-- connection pool is a later refinement.
module Cardano.Server.Http
  ( runServer
  )
where

import Cardano.Api
  ( AsType (AsAddressAny)
  , TxIn (TxIn)
  , TxIx (TxIx)
  , deserialiseFromRawBytes
  , serialiseAddress
  , serialiseToRawBytes
  )

import Cardano.Sieve.Selector
  ( Selector (..)
  , credentialHashToBytes
  , selectorFromText
  )
import Cardano.Sieve.Value (decodeValue)

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
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
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64, Word8)
import Database.SQLite.Simple
  ( Connection
  , Only (Only)
  , Query (Query)
  , SQLData (SQLBlob, SQLInteger)
  , query
  , query_
  , withConnection
  , (:.) ((:.))
  )
import GHC.Clock (getMonotonicTime)
import Network.Wai (Middleware, rawPathInfo, rawQueryString, requestMethod)
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (die)
import System.Posix.Files (fileExist)

import Servant
  ( Capture
  , CaptureAll
  , Get
  , Handler
  , JSON
  , QueryFlag
  , QueryParam
  , Server
  , ServerError (errBody)
  , err400
  , serve
  , throwError
  , (:<|>) ((:<|>))
  , (:>)
  )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

-- | The query API.
type API = MatchesAPI :<|> PreimageAPI

-- | Preimage lookups by hash: the bodies behind the hashes a match reports.
--
-- Both return @null@ rather than a 404 for an unknown hash, as kupo does — a
-- referenced datum whose body has not been seen on chain is a normal state, not an
-- error.
type PreimageAPI =
  "datums" :> Capture "datum-hash" Text :> Get '[JSON] Value
    :<|> "scripts" :> Capture "script-hash" Text :> Get '[JSON] Value

type MatchesAPI =
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
    :> Get '[JSON] [Value]

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
        -- EXISTS rather than a join: this narrows rows the pattern already chose, and
        -- must not multiply them when an output holds several matching assets.
        Right
          [
            ( "EXISTS (SELECT 1 FROM policies pf \
              \WHERE pf.output_reference = u.output_reference \
              \AND pf.policy_num = (SELECT policy_num FROM policy_ids WHERE policy_id = ?)"
                <> maybe "" (const " AND pf.asset_name = ?") name
                <> ")"
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
runServer :: FilePath -> Int -> IO ()
runServer dbPath port = do
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
  Warp.run port (logRequests (serve (Proxy :: Proxy API) (server dbPath)))

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
    try (withConnection dbPath probe) :: IO (Either SomeException (Int, Maybe Int64, Bool))
  case probed of
    Left err ->
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
    rows <- query_ conn "SELECT count(*) FROM unspent"
    tips <- query_ conn "SELECT max(created_slot) FROM unspent"
    -- One representative deferred index; they are all installed together.
    idxs <-
      query_
        conn
        "SELECT count(*) FROM sqlite_master WHERE type='index' AND name='unspentByAddress'"
    pure (headOr 0 rows, headOr Nothing tips, headOr (0 :: Int) idxs > 0)

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

server :: FilePath -> Server API
server dbPath =
  matches :<|> (datumByHash dbPath :<|> scriptByHash dbPath)
 where
  -- Servant delivers the eight filter parameters positionally; group them into the
  -- two records here so nothing downstream handles eight loose Maybes in a row.
  matches segs unspent spent resolve cAfter cBefore sAfter sBefore pol asset tx ix =
    matchesByPattern
      dbPath
      segs
      unspent
      spent
      resolve
      (SlotBounds cAfter cBefore sAfter sBefore)
      (Refinements pol asset tx ix)

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
    Right raw -> liftIO $ withConnection dbPath $ \conn -> do
      rows <- query conn sql (Only raw)
      pure $ case rows of
        Only body : _ -> render body
        [] -> Null

-- | How many matches one request returns. Kupo streams every match; we page, so
-- a hot key cannot turn one request into a multi-hundred-megabyte response.
-- Cursor pagination is the follow-up that closes the parity gap.
pageLimit :: Int
pageLimit = 100

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
  -> Bool
  -> Bool
  -> SlotBounds
  -> Refinements
  -> Maybe Order
  -> Handler [Value]
matchesByPattern dbPath segments unspentFlag spentFlag resolveHashes bounds refine order = do
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
  let desc = fromMaybe MostRecentFirst order == MostRecentFirst
  liftIO $ withConnection dbPath $ \conn -> do
    rows <-
      query
        conn
        (planSql plan filters desc)
        (planParams plan <> concatMap snd filters)
    pure (map (rowToJson resolveHashes) rows)
 where
  -- One row shape for every status, so a single 'rowToJson' serves them all. The
  -- spends join is a primary-key probe that yields nothing on the @unspent@ base
  -- (those rows are deleted when spent), which at 'pageLimit' rows is immaterial.
  planSql plan filters desc =
    "SELECT u.output_reference, u.transaction_index, u.address, u.value, u.datum_hash, \
    \u.datum_type, u.reference_script_hash, u.created_slot, bc.header_hash, \
    \s.spent_slot, bs.header_hash, s.spending_transaction_id, \
    \s.spending_input_index, s.redeemer, "
      <> (if resolveHashes then "bd.datum, sc.script" else "NULL, NULL")
      <> " FROM "
      <> planFrom plan
      <> " LEFT JOIN blocks bc ON bc.slot_no = u.created_slot \
         \LEFT JOIN spends s ON s.output_reference = u.output_reference \
         \LEFT JOIN blocks bs ON bs.slot_no = s.spent_slot"
      -- Only joined when asked for: both are primary-key probes, but resolving on
      -- every match would ship a datum body per row (and one popular datum is
      -- referenced by 2,480 outputs, so a large page would repeat it).
      <> ( if resolveHashes
             then
               " LEFT JOIN binary_data bd ON bd.datum_hash = u.datum_hash \
               \LEFT JOIN scripts sc ON sc.script_hash = u.reference_script_hash"
             else ""
         )
      <> " WHERE "
      <> planWhere plan
      <> foldMap (\(cond, _) -> " AND " <> cond) filters
      <> " ORDER BY "
      <> planSlotCol plan
      <> (if desc then " DESC" else " ASC")
      <> " LIMIT "
      <> Query (pack (show pageLimit))

  badRequest :: Text -> Handler a
  badRequest msg = throwError err400{errBody = LBS.fromStrict (encodeUtf8 msg)}

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
  SelectAll _ -> Right (onBase "1" [])
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
    OnlySpent -> " AND s.output_reference IS NOT NULL"
    AllMatches -> ""

  onBase cond params = Plan baseTable (cond <> statusFilter) "u.created_slot" params

  -- The policy hash is resolved to its small policy_ids surrogate by a scalar
  -- subquery, which keeps a single-value equality on the leading column of
  -- policiesByPolicyId / policiesByAssetId.
  viaPolicies extra params =
    Plan
      (baseTable <> " JOIN policies p ON p.output_reference = u.output_reference")
      ( "p.policy_num = (SELECT policy_num FROM policy_ids WHERE policy_id = ?)"
          <> extra
          <> statusFilter
      )
      "p.created_slot"
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
  :: Bool
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
          <> [ "datum" .= (hexText <$> mDatumBody) | resolved
             ]
          <> [ "script" .= (scriptBodyJson =<< mScriptBody) | resolved
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
