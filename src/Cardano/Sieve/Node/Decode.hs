{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | The decode + sieve stage: turn a decoded block into the outputs that match
-- the configured selectors, ready to persist.
--
-- It bridges the pure matcher ("Cardano.Sieve.Selector") and the write path
-- ("Cardano.Sieve.Node.Insert"):
--
--   * 'outputsInBlock' walks a block's transactions and builds a
--     'DecodedOutput' per output (the minimal decode the matcher and schema
--     need). Byron blocks yield no transactions ('getBlockTxs' returns @[]@).
--   * 'selectedOutputs' keeps the outputs that satisfy /any/ selector.
--   * 'selectedStored' serialises those to 'StoredOutput's for the writer.
--
-- Each output is read as the experimental 'TxOut' (a wrapper over the ledger
-- output), and its fields are read with ledger lenses: address via
-- 'addrTxOutL', value via 'valueTxOutL', the datum via 'datumTxOutF', and the
-- reference script via 'referenceScriptTxOutL'. Datums and reference scripts
-- only exist from Alonzo and Babbage onwards respectively, so those reads sit
-- inside the matching 'ShelleyBasedEra' branches (Alonzo+ and Babbage+).
--
-- Value encoding: @cardano-api@'s 'Value' has no raw-bytes/CBOR instance, so
-- 'toStored' serialises it as JSON (the database is wipe-and-resync for now; a
-- compact ledger-CBOR encoding is a later refinement).
module Cardano.Sieve.Node.Decode
  ( DecodedOutput (..)
  , outputsInBlock
  , toContext
  , selectedOutputs
  , selectedStored
  , spentInputs
  )
where

import Cardano.Api
  ( AddressAny
  , AssetId (AssetId)
  , BlockInMode (BlockInMode)
  , ShelleyBasedEra (..)
  , Tx (ShelleyTx)
  , TxIn (TxIn)
  , TxIx (TxIx)
  , Value
  , fromLedgerValue
  , fromShelleyAddrToAny
  , fromShelleyScriptHash
  , fromShelleyTxIn
  , getBlockTxs
  , getTxIdShelley
  , serialiseToRawBytes
  , shelleyBasedEraConstraints
  )
import Cardano.Api.Experimental.Tx (TxOut (TxOut))
import Cardano.Api.Ledger qualified as L

import Cardano.Sieve.Node.Insert (SpentInput (..), StoredOutput (..))
import Cardano.Sieve.Selector
  ( OutputContext (..)
  , Selector
  , delegationHash
  , paymentHash
  , satisfies
  )

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Exts (toList)
import Lens.Micro ((^.))

-- | Everything one output contributes, decoded once: the fields
-- 'Cardano.Sieve.Selector.satisfies' reads (via 'toContext') plus the extra
-- hashes the schema persists. The creation slot is not here — it is a property
-- of the whole block and is supplied at write time.
data DecodedOutput = DecodedOutput
  { doOutputRef :: TxIn
  , doAddress :: AddressAny
  , doValue :: Value
  , doDatumHash :: Maybe ByteString
  , doReferenceScriptHash :: Maybe ByteString
  , doMetadataTags :: Set Word64
  -- ^ Top-level metadata labels on the producing transaction (shared by all its
  -- outputs); empty if none.
  }

-- | Every output of every transaction in a block. Byron blocks contribute
-- nothing ('getBlockTxs' is @[]@ for them).
outputsInBlock :: BlockInMode -> [DecodedOutput]
outputsInBlock (BlockInMode _ block) = concatMap txOutputs (getBlockTxs block)

-- | Decode every output of one transaction. Each output carries the
-- transaction's id (in its output reference) and the set of metadata labels on
-- the transaction.
txOutputs :: Tx era -> [DecodedOutput]
txOutputs (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $
    let txid = getTxIdShelley sbe (ledgerTx ^. L.bodyTxL)

        tags = case L.strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL) of
          Nothing -> Set.empty
          Just aux -> Map.keysSet (aux ^. L.metadataTxAuxDataL)

        -- Datums exist from Alonzo onwards; 'datumTxOutF' yields the full
        -- datum, so an inline datum is hashed rather than reported as absent.
        datumHash o = case sbe of
          ShelleyBasedEraShelley -> Nothing
          ShelleyBasedEraAllegra -> Nothing
          ShelleyBasedEraMary -> Nothing
          ShelleyBasedEraAlonzo -> hashDatum (o ^. L.datumTxOutF)
          ShelleyBasedEraBabbage -> hashDatum (o ^. L.datumTxOutF)
          ShelleyBasedEraConway -> hashDatum (o ^. L.datumTxOutF)
         where
          hashDatum d = case d of
            L.NoDatum -> Nothing
            L.DatumHash dh -> Just (L.hashToBytes (L.extractHash dh))
            L.Datum bd -> Just (L.hashToBytes (L.extractHash (L.hashBinaryData bd)))

        -- Reference scripts exist from Babbage onwards.
        refScriptHash o = case sbe of
          ShelleyBasedEraShelley -> Nothing
          ShelleyBasedEraAllegra -> Nothing
          ShelleyBasedEraMary -> Nothing
          ShelleyBasedEraAlonzo -> Nothing
          ShelleyBasedEraBabbage -> hashRefScript (o ^. L.referenceScriptTxOutL)
          ShelleyBasedEraConway -> hashRefScript (o ^. L.referenceScriptTxOutL)
         where
          hashRefScript ms = case ms of
            L.SNothing -> Nothing
            L.SJust s -> Just (serialiseToRawBytes (fromShelleyScriptHash (L.hashScript s)))

        mkOutput ix (TxOut o) =
          DecodedOutput
            { doOutputRef = TxIn txid (TxIx ix)
            , doAddress = fromShelleyAddrToAny (o ^. L.addrTxOutL)
            , doValue = fromLedgerValue sbe (o ^. L.valueTxOutL)
            , doDatumHash = datumHash o
            , doReferenceScriptHash = refScriptHash o
            , doMetadataTags = tags
            }
     in zipWith mkOutput [0 ..] (TxOut <$> F.toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL))

-- | Project the matcher's view out of a decoded output.
toContext :: DecodedOutput -> OutputContext
toContext o =
  OutputContext
    { ocOutputRef = doOutputRef o
    , ocAddress = doAddress o
    , ocValue = doValue o
    , ocMetadataTags = doMetadataTags o
    }

-- | The outputs of a block that satisfy /any/ configured selector.
selectedOutputs :: [Selector] -> BlockInMode -> [DecodedOutput]
selectedOutputs selectors blk =
  filter (\o -> any (satisfies (toContext o)) selectors) (outputsInBlock blk)

-- | The selected outputs of a block, serialised for the writer.
selectedStored :: [Selector] -> BlockInMode -> [StoredOutput]
selectedStored selectors = map toStored . selectedOutputs selectors

-- | Every consumed input of every transaction in a block. These are surfaced
-- for /all/ inputs (an input carries no data to run a selector against); the
-- writer keeps only the ones whose consumed output is tracked.
spentInputs :: BlockInMode -> [SpentInput]
spentInputs (BlockInMode _ block) = concatMap txSpends (getBlockTxs block)

-- | The consumed inputs of one transaction, each tagged with the spending
-- transaction's id and the input's index.
txSpends :: Tx era -> [SpentInput]
txSpends (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $
    let txid = serialiseToRawBytes (getTxIdShelley sbe (ledgerTx ^. L.bodyTxL))
     in zipWith
          ( \ix li ->
              SpentInput
                { siConsumed = encodeOutputRef (fromShelleyTxIn li)
                , siSpendingTxId = txid
                , siInputIndex = ix
                }
          )
          [0 ..]
          (F.toList (ledgerTx ^. L.bodyTxL . L.inputsTxBodyL))

-- | Serialise a selected output to the bytes the schema stores.
toStored :: DecodedOutput -> StoredOutput
toStored o =
  StoredOutput
    { soOutputRef = encodeOutputRef (doOutputRef o)
    , soAddress = serialiseToRawBytes (doAddress o)
    , soPayCred = paymentHash (doAddress o)
    , soDelegCred = delegationHash (doAddress o)
    , soValue = LBS.toStrict (encode (doValue o))
    , soDatumHash = doDatumHash o
    , soReferenceScriptHash = doReferenceScriptHash o
    , soPolicyIds = policyIdsOf (doValue o)
    }

-- | Encode an output reference as the transaction id bytes followed by the
-- output index as a big-endian 'Word64'. Fixed-width and order-preserving.
encodeOutputRef :: TxIn -> ByteString
encodeOutputRef (TxIn txid (TxIx ix)) =
  serialiseToRawBytes txid
    <> LBS.toStrict (toLazyByteString (word64BE (fromIntegral ix)))

-- | The distinct policy ids of the positive-quantity assets in a value (ada
-- excluded).
policyIdsOf :: Value -> [ByteString]
policyIdsOf v =
  Set.toList
    ( Set.fromList
        [serialiseToRawBytes pid | (AssetId pid _, q) <- toList v, q > 0]
    )
