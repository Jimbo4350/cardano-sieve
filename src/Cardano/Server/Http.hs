{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Read query API (servant + warp) over the synced SQLite database.
--
-- One endpoint, @GET \/matches\/{pattern}?unspent@, covering every dimension the
-- indexer can match on. The pattern is parsed by
-- 'Cardano.Sieve.Selector.selectorFromText' — the same grammar @--select@ uses at
-- ingest — and 'planFor' maps the resulting 'Selector' onto the index that serves
-- it, so query syntax and ingest syntax cannot drift:
--
--   * @*@ — wildcard                      * @{payment}\/{delegation}@ — address parts
--   * a bech32\/base58\/base16 address     * @*\@{txid}@ — a whole transaction
--   * @{policy}.{name}@ \/ @{policy}.*@    * @{index}\@{txid}@ — one output
--
-- Kupo-compatible query parameters: @?unspent@, @?spent@, @?created_after@,
-- @?created_before@, @?order=most_recent_first|oldest_first@. As in kupo, passing
-- neither status flag returns both spent and unspent matches.
--
-- Which table answers a request is decided by 'planFor': @?unspent@ reads the
-- indexed live set, anything spent-inclusive reads full history. Policy and asset
-- stay index-served either way because @policies@ is itself full-history.
--
-- Response shape is field-for-field identical to kupo's, verified by diffing
-- pinned rows of every kind (datum by hash, inline datum, no datum, spent,
-- reference script).
--
-- Known gaps against kupo: results are capped at 'pageLimit' where kupo streams
-- every match; @spent_at.redeemer@ is always null because the write path does not
-- populate that column yet; and within one slot the result order differs, since
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
import Data.Aeson (Value (Null), object, (.=))
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
import Data.Word (Word64)
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
  ( CaptureAll
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
  , (:>)
  )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

-- | The query API: one endpoint, every dimension.
type API =
  "matches"
    :> CaptureAll "pattern" Text
    :> QueryFlag "unspent"
    :> QueryFlag "spent"
    :> QueryParam "created_after" Int64
    :> QueryParam "created_before" Int64
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
server = matchesByPattern

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
  -> Maybe Int64
  -> Maybe Int64
  -> Maybe Order
  -> Handler [Value]
matchesByPattern dbPath segments unspentFlag spentFlag createdAfter createdBefore order = do
  -- The pattern is captured as PATH SEGMENTS and rejoined, because the
  -- payment/delegation form embeds a '/' and so spans two segments. kupo does the
  -- same (it matches on @"matches" : args@).
  let pat = T.intercalate "/" segments
  when (null segments) $
    badRequest "no pattern given: try /matches/*"
  status <- either badRequest pure (statusFromFlags unspentFlag spentFlag)
  selector <- case selectorFromText pat of
    Left err -> badRequest ("invalid pattern: " <> pack (show err))
    Right s -> pure s
  plan <- either badRequest pure (planFor status selector)
  let desc = fromMaybe MostRecentFirst order == MostRecentFirst
  liftIO $ withConnection dbPath $ \conn -> do
    rows <- query conn (planSql plan desc) (planParams plan <> slotParams)
    pure (map rowToJson rows)
 where
  slotBounds =
    [(">=", v) | Just v <- [createdAfter]] <> [("<=", v) | Just v <- [createdBefore]]
  slotParams = [SQLInteger v | (_, v) <- slotBounds]

  -- One row shape for every status, so a single 'rowToJson' serves them all. The
  -- spends join is a primary-key probe that yields nothing on the @unspent@ base
  -- (those rows are deleted when spent), which at 'pageLimit' rows is immaterial.
  planSql plan desc =
    "SELECT u.output_reference, u.transaction_index, u.address, u.value, u.datum_hash, \
    \u.datum_type, u.reference_script_hash, u.created_slot, bc.header_hash, \
    \s.spent_slot, bs.header_hash, s.spending_transaction_id, \
    \s.spending_input_index, s.redeemer FROM "
      <> planFrom plan
      <> " LEFT JOIN blocks bc ON bc.slot_no = u.created_slot \
         \LEFT JOIN spends s ON s.output_reference = u.output_reference \
         \LEFT JOIN blocks bs ON bs.slot_no = s.spent_slot WHERE "
      <> planWhere plan
      <> foldMap (\(op, _) -> " AND " <> planSlotCol plan <> " " <> Query op <> " ?") slotBounds
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
  serialiseToRawBytes txid
    <> LBS.toStrict (toLazyByteString (word64BE (fromIntegral ix)))

-- | One row as JSON, mirroring kupo's match shape field for field.
rowToJson
  :: ( ByteString
     , Int64
     , ByteString
     , ByteString
     , Maybe ByteString
     , Maybe Int64
     , Maybe ByteString
     , Int64
     , Maybe ByteString
     )
    :. (Maybe Int64, Maybe ByteString, Maybe ByteString, Maybe Int64, Maybe ByteString)
  -> Value
rowToJson
  ( (oref, txIx, addr, val, mDatum, mDatumType, mScript, slot, mHeader)
      :. (mSpentSlot, mSpentHeader, mSpendTx, mInputIx, mRedeemer)
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
