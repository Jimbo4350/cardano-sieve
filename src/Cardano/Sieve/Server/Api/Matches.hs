{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/matches@ — the query surface over everything the indexer stored.
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
-- All thirteen @\/matches@ parameters: @?unspent@, @?spent@,
-- @?resolve_hashes@, @?order@, @?created_after@, @?created_before@,
-- @?spent_after@, @?spent_before@, @?policy_id@, @?asset_name@,
-- @?transaction_id@, @?output_index@, plus the pattern itself. Passing neither
-- status flag returns both spent and unspent. Bare @\/matches@ with no
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
-- Sieve serves 'pageLimit' rows per request with an @X-Next-Cursor@ header
-- \/ @?after@ parameter to walk the rest — every match is reachable. Within
-- one slot the order is the total @(created_slot, rowid)@. A fresh read
-- connection is opened per request; a connection pool is a later refinement.
module Cardano.Sieve.Server.Api.Matches
  ( MatchesAPI
  , matchesServer

    -- * Exposed for the test suite
  , Cursor (..)
  , cursorToText
  , cursorFromText
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
  ( BootstrapFilter (IncludeBootstrap, OnlyShelley)
  , Selector (..)
  , credentialHashToBytes
  , overlaps
  , selectorFromText
  )
import Cardano.Sieve.Server.Api.Common (badRequest, hexText, scriptLanguage, withReadConnection)
import Cardano.Sieve.Value (decodeValue)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Null), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Database.SQLite.Simple
  ( Only (Only)
  , Query (Query)
  , SQLData (SQLBlob, SQLInteger)
  , execute
  , execute_
  , query
  , query_
  , (:.) ((:.))
  )

import Servant
  ( CaptureAll
  , Delete
  , Get
  , Handler
  , Header
  , Headers
  , JSON
  , QueryFlag
  , QueryParam
  , Server
  , addHeader
  , noHeader
  , (:<|>) ((:<|>))
  , (:>)
  )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

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

-- | Both @\/matches@ routes.
matchesServer :: FilePath -> Server MatchesAPI
matchesServer dbPath = matches :<|> matchesDelete dbPath
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

-- | Which way round results come back, from @?order@.
--
-- A real type rather than the raw 'Text' so servant parses and rejects it at the
-- boundary: @?order=sideways@ is a 400 before the handler runs, and the handler
-- cannot forget to validate it. Absent means 'MostRecentFirst'.
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
-- independent: the API allows at most ONE lower bound and ONE upper bound, so
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
-- meaningful in a pair: @asset_name@ can't be used alone and must be
-- provided alongside a @policy_id@, likewise @output_index@ with
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
-- Returns 'Left' on an invalid combination, so an impossible request fails
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

-- | Total mapping from the pair of flags. Neither flag means both sides;
-- both flags at once is a contradiction rather than a default.
statusFromFlags :: Bool -> Bool -> Either Text Status
statusFromFlags unspent spent = case (unspent, spent) of
  (True, True) -> Left "?spent and ?unspent are mutually exclusive"
  (True, False) -> Right OnlyUnspent
  (False, True) -> Right OnlySpent
  (False, False) -> Right AllMatches

-- | @DELETE \/matches\/{pattern}@ — prune everything the pattern matched:
-- the outputs, their live-set rows, their spend records and their policy index
-- rows, in one transaction, answering @{"deleted": n}@ with the count of
-- outputs removed. This is how disk is reclaimed after narrowing interest —
-- e.g. having indexed under @*@ and later caring about one address — without
-- a wipe-and-resync.
--
-- Refused ('overlaps') while a configured selector still covers the pattern:
-- the indexer would re-create the rows from the next block, so the delete
-- would be pointless churn. Remove the selector from
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

-- | How many matches one request returns. We page, so a hot key cannot turn
-- one request into a multi-hundred-megabyte response.
-- A full page carries an @X-Next-Cursor@ header; @?after@ with its value
-- resumes exactly where the page stopped, so every match is reachable — a
-- client must walk pages rather than read one unbounded body.
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
-- reorg is best-effort.
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
  -- payment/delegation form embeds a '/' and so spans two segments. No segments
  -- at all is the bare /matches route, which is treated as the wildcard.
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
  -- assets of a policy came back six times, 14,780 rows.
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

-- | One row as JSON.
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
        , "value" .= valueJson val
        , "datum_hash" .= (hexText <$> mDatum)
        , "script_hash" .= (hexText <$> mScript)
        , "created_at" .= object ["slot_no" .= slot, "header_hash" .= (hexText <$> mHeader)]
        , "spent_at" .= spentAt
        ]
          -- Present only when there IS a datum: the key is omitted entirely
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
-- discriminator byte — the same shape @\/scripts\/{hash}@ returns.
scriptBodyJson :: ByteString -> Maybe Value
scriptBodyJson body = case BS.uncons body of
  Nothing -> Nothing
  Just (tag, raw) -> Just (object ["script" .= hexText raw, "language" .= scriptLanguage tag])

-- | Render the stored datum-type flag. Anything other than the
-- two known encodings yields 'Nothing' rather than a guess, so a future third
-- kind cannot be silently mislabelled as one of these.
datumTypeText :: Int64 -> Maybe Text
datumTypeText = \case
  0 -> Just "hash"
  1 -> Just "inline"
  _ -> Nothing

-- | The output reference is 32 tx-id bytes then a big-endian Word64 output
-- index; recover the index.
outputIndex :: ByteString -> Word64
outputIndex = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 . BS.drop 32

-- | Render the raw address bytes (bech32 for Shelley, base58 for
-- Byron); fall back to hex if it does not decode.
addressText :: ByteString -> Text
addressText raw = case deserialiseFromRawBytes AsAddressAny raw of
  Right a -> serialiseAddress a
  Left _ -> hexText raw

-- | Reshape a stored value into the wire shape @{coins, assets:{"policy.name":qty}}@,
-- quantities rendered as strings.
--
-- The stored form is the compact CBOR of "Cardano.Sieve.Value"; an asset with an
-- empty name renders as just the bare policy id. A value that fails
-- to decode yields zero coins and no assets rather than failing the request —
-- a single unreadable row should not take out a whole page of results.
valueJson :: ByteString -> Value
valueJson raw = case decodeValue raw of
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
