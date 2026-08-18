{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compact CBOR encoding for an output's value, and the matching decoder.
--
-- Both directions live in this one module deliberately: they are a matched pair
-- and a drift between them is silent data loss. The write path
-- ("Cardano.Sieve.Node.Decode") uses 'encodeValue'; the query path
-- ("Cardano.Sieve.Server.Api.Matches") uses 'decodeValue'.
--
-- == Why not JSON
--
-- Values were previously stored as @aeson@ JSON, which is the single largest
-- column in the database: 218.6 MB of a 1.48 GB preview sync to slot 4,000,000,
-- averaging 134 bytes per output. JSON pays for field names and decimal digits on
-- every row, and every output pays the encode on the ingest hot path.
--
-- == The format
--
-- The ledger's own @MaryValue@ shape:
--
--   * ada-only: a bare CBOR unsigned integer (the lovelace amount).
--     @1A00989680@ — 5 bytes, against 19 for @{"lovelace":10000000}@.
--   * with assets: a 2-element array @[coins, {policy: {name: quantity}}]@,
--     policy ids and asset names as CBOR byte strings.
--
-- Maps are emitted with keys in ascending byte order, definite length.
--
-- Measured at preview origin..2,000,000, 281,263 outputs: the value column went
-- 45,196,241 bytes of JSON to 21,856,001 — a 52% cut.
--
-- In the asset map, lengths are definite. CBOR packs a map's entry
-- count into the head byte for counts 0..23, so a small map costs one byte
-- (@A1@ = 1 entry); from 24 up the count needs a following byte (@B8 1B@ = 27
-- entries). Indefinite length always costs two (@BF@ start, @FF@ break). So below
-- 24 entries definite length is strictly smaller; at 24 and above the two forms
-- tie at two bytes.
--
-- Consequence: the choice only matters for outputs holding 24 or more asset
-- names under a single policy — 65 distinct blobs in the whole 2M range.
-- Definite length is the more canonical choice, and 'decodeValue' accepts both
-- forms, so a value written by either encoder reads back the same.
module Cardano.Sieve.Value
  ( encodeValue
  , decodeValue
  )
where

import Cardano.Api
  ( AssetId (AdaAssetId, AssetId)
  , Quantity (Quantity)
  , Value
  , serialiseToRawBytes
  )

import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Read qualified as R
import Codec.CBOR.Write qualified as W
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Exts (toList)

-- | Serialise a value to the compact CBOR the schema stores.
--
-- Only positive quantities are kept: a value in an output cannot legitimately
-- carry a zero or negative asset quantity, and dropping them keeps the encoding
-- canonical (the ledger does not represent them either).
encodeValue :: Value -> ByteString
encodeValue v = W.toStrictByteString (encodeCoinAssets ada assets)
 where
  ada = sum [q | (AdaAssetId, Quantity q) <- toList v]
  assets =
    Map.fromListWith
      (Map.unionWith (+))
      [ (serialiseToRawBytes pid, Map.singleton (serialiseToRawBytes name) q)
      | (AssetId pid name, Quantity q) <- toList v
      , q > 0
      ]

encodeCoinAssets :: Integer -> Map ByteString (Map ByteString Integer) -> E.Encoding
encodeCoinAssets ada assets
  | Map.null assets = E.encodeInteger ada
  | otherwise =
      E.encodeListLen 2
        <> E.encodeInteger ada
        <> encodeByteMap id (encodeByteMap E.encodeInteger <$> assets)

-- | A definite-length CBOR map keyed by byte strings, in ascending key order.
-- 'Map.toAscList' already gives that order, since 'ByteString' compares
-- lexicographically.
encodeByteMap :: (v -> E.Encoding) -> Map ByteString v -> E.Encoding
encodeByteMap encVal m =
  E.encodeMapLen (fromIntegral (Map.size m))
    <> foldMap (\(k, v) -> E.encodeBytes k <> encVal v) (Map.toAscList m)

-- | Recover the lovelace amount and the assets from a stored value.
--
-- Returns the assets flattened to @(policy id, asset name, quantity)@, sorted by
-- policy then name, which is the order the query layer renders them in. 'Left'
-- carries a human-readable reason so a decode failure is diagnosable rather than
-- silently becoming an empty value.
decodeValue :: ByteString -> Either String (Integer, [(ByteString, ByteString, Integer)])
decodeValue bs =
  case R.deserialiseFromBytes valueDecoder (LBS.fromStrict bs) of
    Left err -> Left (show err)
    Right (rest, r)
      | LBS.null rest -> Right r
      | otherwise -> Left ("trailing bytes after value: " <> show (LBS.length rest))

valueDecoder :: D.Decoder s (Integer, [(ByteString, ByteString, Integer)])
valueDecoder = do
  tk <- D.peekTokenType
  case tk of
    -- Ada-only values are written as a bare integer, so accept that shape first.
    D.TypeUInt -> adaOnly
    D.TypeUInt64 -> adaOnly
    D.TypeInteger -> adaOnly
    _ -> do
      _ <- D.decodeListLen
      ada <- D.decodeInteger
      assets <- decodeByteMap (decodeByteMap D.decodeInteger)
      pure
        ( ada
        , sortOn
            (\(p, n, _) -> (p, n))
            [(pid, name, q) | (pid, names) <- assets, (name, q) <- names]
        )
 where
  adaOnly = fmap (\ada -> (ada, [])) D.decodeInteger

-- | A CBOR map keyed by byte strings. Handles both the definite-length form
-- 'encodeByteMap' writes and the indefinite-length form other encoders may emit,
-- so a value written by a different tool still decodes.
decodeByteMap :: D.Decoder s v -> D.Decoder s [(ByteString, v)]
decodeByteMap decVal = do
  mLen <- D.decodeMapLenOrIndef
  case mLen of
    Just n -> replicateEntry n
    Nothing -> untilBreak
 where
  entry = (,) <$> D.decodeBytes <*> decVal
  replicateEntry n
    | n <= 0 = pure []
    | otherwise = (:) <$> entry <*> replicateEntry (n - 1)
  untilBreak = do
    stop <- D.decodeBreakOr
    if stop then pure [] else (:) <$> entry <*> untilBreak
