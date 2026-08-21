{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Decode a block's outputs and keep the ones that match the configured
-- selectors: 'outputsInBlock' decodes, 'selectedOutputs' filters.
module Cardano.Sieve.Node.Decode
  ( DecodedOutput (..)
  , outputsInBlock
  , datumsAndScriptsInBlock
  , toContext
  , selectedOutputs
  , spentInputs
  , encodeOutputRef
  )
where

import Cardano.Api
  ( AddressAny
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

import Cardano.Ledger.Alonzo.Core (originalBytes, scriptPrefixTag)
import Cardano.Ledger.Alonzo.Scripts
  ( AsIx (AsIx)
  , mkSpendingPurpose
  )
import Cardano.Ledger.Alonzo.Tx (IsValid (..), isValidTxL)
import Cardano.Ledger.Alonzo.TxWits (datsTxWitsL, rdmrsTxWitsL, unRedeemers, unTxDats)
import Cardano.Ledger.Babbage.TxBody (collateralReturnTxBodyL)
import Cardano.Sieve.Node.Insert
  ( DatumType (DatumByHash, DatumInline)
  , DatumsAndScripts (..)
  , RedeemerCapture (CaptureRedeemers, SkipRedeemers)
  , SpentInput (..)
  )
import Cardano.Sieve.Selector
  ( OutputContext (..)
  , Selector
  , satisfies
  )

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Lens.Micro ((^.))

-- | Everything one output contributes, decoded once: the fields
-- 'Cardano.Sieve.Selector.satisfies' reads (via 'toContext') plus the extra
-- hashes the schema persists. The creation slot is not here — it is a property
-- of the whole block and is supplied at write time.
data DecodedOutput = DecodedOutput
  { doOutputRef :: TxIn
  , doTransactionIndex :: Word64
  -- ^ Position of the producing transaction within its block.
  , doAddress :: AddressAny
  , doValue :: Value
  , doDatum :: Maybe (DatumType, ByteString)
  -- ^ The datum's type and hash, when the output carries one.
  , doReferenceScriptHash :: Maybe ByteString
  , doMetadataTags :: Set Word64
  -- ^ Metadata labels of the producing transaction. Metadata is per-transaction,
  -- so every output of the same transaction carries the same set; empty if none.
  }

-- | Every output of every transaction in a block. Byron blocks contribute
-- nothing ('getBlockTxs' is @[]@ for them).
outputsInBlock :: BlockInMode -> [DecodedOutput]
outputsInBlock (BlockInMode _ block) =
  concat (zipWith outputsInTx [0 ..] (getBlockTxs block))

-- | Decode every output of one transaction. Each output carries the
-- transaction's id (in its output reference), its position within the block, and
-- the set of metadata labels on the transaction.
--
-- The position comes from the caller rather than the transaction itself: nothing
-- on a transaction records where in its block it sits, so it is the enumeration
-- order of 'getBlockTxs' — which is the block's own transaction order, and so
-- the index reported as @transaction_index@ in match responses.
outputsInTx :: Word64 -> Tx era -> [DecodedOutput]
outputsInTx txIx (ShelleyTx sbe ledgerTx) =
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
          -- 'datumTxOutF' yields the FULL datum, so an inline datum is hashed here
          -- rather than reported as absent — and which branch we took is exactly
          -- the stored @datum_type@.
          hashDatum d = case d of
            L.NoDatum -> Nothing
            L.DatumHash dh -> Just (DatumByHash, L.hashToBytes (L.extractHash dh))
            L.Datum bd ->
              Just (DatumInline, L.hashToBytes (L.extractHash (L.hashBinaryData bd)))

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
            , doTransactionIndex = txIx
            , doAddress = fromShelleyAddrToAny (o ^. L.addrTxOutL)
            , doValue = fromLedgerValue sbe (o ^. L.valueTxOutL)
            , doDatum = datumHash o
            , doReferenceScriptHash = refScriptHash o
            , doMetadataTags = tags
            }
        txOuts = F.toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL)
        txOutputs = zipWith mkOutput [0 ..] (TxOut <$> txOuts)
     in -- Every era enumerated (no wildcard) so a future era must be handled
        -- explicitly, not silently take the pre-Babbage path. On a
        -- Babbage/Conway isValid=false (phase-2 failure) tx the transaction's
        -- outputs were never created; the only created output is the collateral
        -- return, at index = number of outputs. The collateral return is
        -- optional: SNothing is a tx that declared none, forfeiting the whole
        -- collateral to fees and creating no output at all.
        case sbe of
          ShelleyBasedEraShelley -> txOutputs
          ShelleyBasedEraAllegra -> txOutputs
          ShelleyBasedEraMary -> txOutputs
          ShelleyBasedEraAlonzo -> txOutputs
          ShelleyBasedEraBabbage -> case ledgerTx ^. isValidTxL of
            IsValid True -> txOutputs
            IsValid False -> case ledgerTx ^. L.bodyTxL . collateralReturnTxBodyL of
              L.SJust o -> [mkOutput (fromIntegral (length txOuts)) (TxOut o)]
              L.SNothing -> []
          ShelleyBasedEraConway -> case ledgerTx ^. isValidTxL of
            IsValid True -> txOutputs
            IsValid False -> case ledgerTx ^. L.bodyTxL . collateralReturnTxBodyL of
              L.SJust o -> [mkOutput (fromIntegral (length txOuts)) (TxOut o)]
              L.SNothing -> []

-- | Every datum and script a block carries.
--
-- Four sources: witness-set datums and witness-set/auxiliary scripts, plus the
-- /inline/ datum and the /reference script/ straight off each output, where the
-- bytes are already in hand for free.
--
-- The collection here is unconditional; it is 'applyBlock' that decides
-- whether to write, and it writes a block's datums and scripts only when the
-- block produced a matched output. Without that condition every datum and
-- script on the chain would be stored no matter how narrow the selectors.
datumsAndScriptsInBlock :: BlockInMode -> DatumsAndScripts
datumsAndScriptsInBlock (BlockInMode _ block) = foldMap datumsAndScriptsInTx (getBlockTxs block)

-- | The datums and scripts of one transaction.
datumsAndScriptsInTx :: Tx era -> DatumsAndScripts
datumsAndScriptsInTx (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $
    let outs = F.toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL)

        datumsOfWits dats =
          [ (L.hashToBytes (L.extractHash dh), originalBytes d)
          | (dh, d) <- Map.toList (unTxDats dats)
          ]
        datumOfOut d = case d of
          L.Datum bd -> [(L.hashToBytes (L.extractHash (L.hashBinaryData bd)), originalBytes bd)]
          L.DatumHash _ -> []
          L.NoDatum -> []
        -- Datums, per era. Alonzo onwards: witness set plus any inline datum.
        datums = case sbe of
          ShelleyBasedEraShelley -> []
          ShelleyBasedEraAllegra -> []
          ShelleyBasedEraMary -> []
          ShelleyBasedEraAlonzo ->
            datumsOfWits (ledgerTx ^. L.witsTxL . datsTxWitsL)
              <> concatMap (datumOfOut . (^. L.datumTxOutF)) outs
          ShelleyBasedEraBabbage ->
            datumsOfWits (ledgerTx ^. L.witsTxL . datsTxWitsL)
              <> concatMap (datumOfOut . (^. L.datumTxOutF)) outs
          ShelleyBasedEraConway ->
            datumsOfWits (ledgerTx ^. L.witsTxL . datsTxWitsL)
              <> concatMap (datumOfOut . (^. L.datumTxOutF)) outs

        -- Scripts: the witness set in every era, plus output reference scripts
        -- from Babbage. Each stored blob is @scriptPrefixTag s <> originalBytes s@
        -- — exactly the bytes 'L.hashScript' hashes, so hashing a stored script
        -- reproduces its own @script_hash@ column.
        scriptRow sh s =
          (serialiseToRawBytes (fromShelleyScriptHash sh), scriptPrefixTag s <> originalBytes s)
        scripts =
          [scriptRow sh s | (sh, s) <- Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL)]
            <> case sbe of
              ShelleyBasedEraShelley -> []
              ShelleyBasedEraAllegra -> []
              ShelleyBasedEraMary -> []
              ShelleyBasedEraAlonzo -> []
              ShelleyBasedEraBabbage ->
                [scriptRow (L.hashScript s) s | o <- outs, L.SJust s <- [o ^. L.referenceScriptTxOutL]]
              ShelleyBasedEraConway ->
                [scriptRow (L.hashScript s) s | o <- outs, L.SJust s <- [o ^. L.referenceScriptTxOutL]]
     in DatumsAndScripts datums scripts

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

-- | Every consumed input of every transaction in a block. These are surfaced
-- for /all/ inputs (an input carries no data to run a selector against); the
-- writer keeps only the ones whose consumed output is tracked.
spentInputs :: RedeemerCapture -> BlockInMode -> [SpentInput]
spentInputs capture (BlockInMode _ block) =
  concatMap (txSpends capture) (getBlockTxs block)

-- | The consumed inputs of one transaction, each tagged with the spending
-- transaction's id, the input's index, and (when asked for) the redeemer that
-- authorised the spend.
--
-- == On the redeemer's index
--
-- A redeemer is not attached to the input it authorises. It lives in the
-- transaction's witness set, addressed by a /redeemer pointer/ — a purpose plus
-- an index — and for a spend that index is into the inputs as the LEDGER orders
-- them, which is sorted 'TxIn' order, not the order they appear in the
-- transaction's CBOR. Getting that wrong attaches a real redeemer to the wrong
-- input: plausible, and silently incorrect.
--
-- It lines up here because 'L.inputsTxBodyL' is a 'Set', so @F.toList@ already
-- yields sorted order, and 'siInputIndex' is that same enumeration. So pointer
-- index @n@ is exactly the input at @siInputIndex = n@.
txSpends :: RedeemerCapture -> Tx era -> [SpentInput]
txSpends capture (ShelleyTx sbe ledgerTx) =
  shelleyBasedEraConstraints sbe $
    let txid = serialiseToRawBytes (getTxIdShelley sbe (ledgerTx ^. L.bodyTxL))
        regular = F.toList (ledgerTx ^. L.bodyTxL . L.inputsTxBodyL)

        -- On a Babbage/Conway isValid=false tx the regular inputs are NOT
        -- consumed; the collateral inputs are. Pre-Babbage: the regular inputs.
        consumedOf isv collateral = case isv of
          IsValid True -> regular
          IsValid False -> collateral

        consumed = case sbe of
          ShelleyBasedEraShelley -> regular
          ShelleyBasedEraAllegra -> regular
          ShelleyBasedEraMary -> regular
          ShelleyBasedEraAlonzo -> regular
          ShelleyBasedEraBabbage ->
            consumedOf (ledgerTx ^. isValidTxL) (F.toList (ledgerTx ^. L.bodyTxL . L.collateralInputsTxBodyL))
          ShelleyBasedEraConway ->
            consumedOf (ledgerTx ^. isValidTxL) (F.toList (ledgerTx ^. L.bodyTxL . L.collateralInputsTxBodyL))
        -- Redeemers exist from Alonzo onwards. 'mkSpendingPurpose' is a method of
        -- @AlonzoEraScript@, so the call sits inside the concrete branches — but
        -- unlike the per-era constructors it is one expression for all three.
        redeemerAt = case capture of
          SkipRedeemers -> const Nothing
          CaptureRedeemers -> case sbe of
            ShelleyBasedEraShelley -> const Nothing
            ShelleyBasedEraAllegra -> const Nothing
            ShelleyBasedEraMary -> const Nothing
            ShelleyBasedEraAlonzo -> lookupSpend (ledgerTx ^. L.witsTxL . rdmrsTxWitsL)
            ShelleyBasedEraBabbage -> lookupSpend (ledgerTx ^. L.witsTxL . rdmrsTxWitsL)
            ShelleyBasedEraConway -> lookupSpend (ledgerTx ^. L.witsTxL . rdmrsTxWitsL)
     in zipWith
          ( \ix li ->
              SpentInput
                { siConsumed = encodeOutputRef (fromShelleyTxIn li)
                , siSpendingTxId = txid
                , siInputIndex = ix
                , siRedeemer = redeemerAt ix
                }
          )
          [0 ..]
          consumed
 where
  -- Stored as the ledger's own memoised bytes, so what comes back out is what was
  -- on chain rather than a re-encoding of it.
  lookupSpend rdmrs ix =
    originalBytes . fst
      <$> Map.lookup (mkSpendingPurpose (AsIx (fromIntegral ix))) (unRedeemers rdmrs)

-- | Encode an output reference as the transaction id bytes followed by the
-- output index as a big-endian 'Word64'. Fixed-width and order-preserving.
encodeOutputRef :: TxIn -> ByteString
encodeOutputRef (TxIn txid (TxIx ix)) =
  serialiseToRawBytes txid
    <> LBS.toStrict (toLazyByteString (word64BE (fromIntegral ix)))
