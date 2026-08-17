{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | @\/metadata@ — a block's transaction metadata, fetched from the node on
-- demand and never stored (kupo's contract).
module Cardano.Server.Api.Metadata
  ( MetadataAPI
  , metadataServer
  )
where

import Cardano.Api
  ( BlockHeader (BlockHeader)
  , BlockInMode (BlockInMode)
  , ChainPoint (ChainPoint, ChainPointAtGenesis)
  , Hash
  , NetworkId
  , SocketPath
  , Tx (ShelleyTx)
  , TxId
  , deserialiseFromRawBytes
  , deserialiseFromRawBytesHex
  , getBlockHeader
  , getBlockTxs
  , getTxIdShelley
  , proxyToAsType
  , serialiseToRawBytes
  , shelleyBasedEraConstraints
  )
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.Core (TxAuxDataHash (unTxAuxDataHash), hashTxAuxData, originalBytes)
import Cardano.Ledger.Metadata (Metadatum (..))
import Cardano.Server.Api.Common (badRequest, hexText, withReadConnection)
import Cardano.Sieve.Node.FetchBlock (fetchBlockAtSlot)
import Cardano.Slotting.Slot (SlotNo (SlotNo))

import Control.Exception (SomeAsyncException (..), SomeException, fromException, throwIO, try)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Only (Only), query)
import Lens.Micro ((^.))

import Servant
  ( Capture
  , Get
  , Handler
  , Header
  , Headers
  , JSON
  , QueryParam
  , Server
  , ServerError (errBody)
  , addHeader
  , err503
  , noHeader
  , throwError
  , (:>)
  )

-- | On-demand transaction metadata, kupo's contract: never stored, asked of
-- the node per request. See 'metadataBySlot' for the mechanism and the edge
-- cases inherited deliberately.
type MetadataAPI =
  "metadata"
    :> Capture "slot-no" Int64
    :> QueryParam "transaction_id" Text
    :> Get '[JSON] (Headers '[Header "X-Block-Header-Hash" Text] [Value])

-- | The @\/metadata@ route.
metadataServer :: Maybe (SocketPath, NetworkId) -> FilePath -> Server MetadataAPI
metadataServer = metadataBySlot

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
