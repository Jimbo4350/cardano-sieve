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
-- 'toStored' goes through "Cardano.Sieve.Value", which writes the ledger's compact
-- @MaryValue@ CBOR shape.
module Cardano.Sieve.Node.Decode
  ( DecodedOutput (..)
  , outputsInBlock
  , preimagesInBlock
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

-- @originalBytes@ (the memoised CBOR a ledger decoder kept) is not re-exported by
-- @Cardano.Api.Ledger@, same as 'IsValid' and 'collateralReturnTxBodyL'; take it
-- from the ledger directly.
import Cardano.Ledger.Alonzo.Core (originalBytes)
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (NativeScript, PlutusScript)
  , AsIx (AsIx)
  , mkSpendingPurpose
  , plutusScriptLanguage
  )
import Cardano.Ledger.Alonzo.Tx (IsValid (..), isValidTxL)
import Cardano.Ledger.Alonzo.TxWits (datsTxWitsL, rdmrsTxWitsL, unRedeemers, unTxDats)
import Cardano.Ledger.Babbage.TxBody (collateralReturnTxBodyL)
import Cardano.Sieve.Node.Insert
  ( DatumType (DatumByHash, DatumInline)
  , Preimages (..)
  , RedeemerCapture (CaptureRedeemers, SkipRedeemers)
  , SpentInput (..)
  , StoredOutput (..)
  )
import Cardano.Sieve.Selector
  ( OutputContext (..)
  , Selector
  , delegationHash
  , paymentHash
  , satisfies
  )
import Cardano.Sieve.Value (encodeValue)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
  , doTransactionIndex :: Word64
  -- ^ Position of the producing transaction within its block.
  , doAddress :: AddressAny
  , doValue :: Value
  , doDatum :: Maybe (DatumType, ByteString)
  -- ^ The datum's type and hash, when the output carries one.
  , doReferenceScriptHash :: Maybe ByteString
  , doMetadataTags :: Set Word64
  -- ^ Top-level metadata labels on the producing transaction (shared by all its
  -- outputs); empty if none.
  }

-- | Every output of every transaction in a block. Byron blocks contribute
-- nothing ('getBlockTxs' is @[]@ for them).
outputsInBlock :: BlockInMode -> [DecodedOutput]
outputsInBlock (BlockInMode _ block) =
  concat (zipWith txOutputs [0 ..] (getBlockTxs block))

-- | Decode every output of one transaction. Each output carries the
-- transaction's id (in its output reference), its position within the block, and
-- the set of metadata labels on the transaction.
--
-- The position comes from the caller rather than the transaction itself: nothing
-- on a transaction records where in its block it sits, so it is the enumeration
-- order of 'getBlockTxs' — which is the block's own transaction order, and so the
-- same index kupo reports as @transaction_index@.
txOutputs :: Word64 -> Tx era -> [DecodedOutput]
txOutputs txIx (ShelleyTx sbe ledgerTx) =
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
          -- kupo's @datum_type@.
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
        declared = F.toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL)
        declaredOutputs = zipWith mkOutput [0 ..] (TxOut <$> declared)
     in -- Every era enumerated (no wildcard) so a future era must be handled
        -- explicitly, not silently take the pre-Babbage path. On a
        -- Babbage/Conway isValid=false (phase-2 failure) tx the declared outputs
        -- were never created; the only created output is the collateral return,
        -- at index = number of declared outputs. Each branch builds
        -- DecodedOutputs directly (mkOutput applied where the ledger era is
        -- concrete) so the case result carries no era index.
        case sbe of
          ShelleyBasedEraShelley -> declaredOutputs
          ShelleyBasedEraAllegra -> declaredOutputs
          ShelleyBasedEraMary -> declaredOutputs
          ShelleyBasedEraAlonzo -> declaredOutputs
          ShelleyBasedEraBabbage -> case ledgerTx ^. isValidTxL of
            IsValid True -> declaredOutputs
            IsValid False -> case ledgerTx ^. L.bodyTxL . collateralReturnTxBodyL of
              L.SJust o -> [mkOutput (fromIntegral (length declared)) (TxOut o)]
              L.SNothing -> []
          ShelleyBasedEraConway -> case ledgerTx ^. isValidTxL of
            IsValid True -> declaredOutputs
            IsValid False -> case ledgerTx ^. L.bodyTxL . collateralReturnTxBodyL of
              L.SJust o -> [mkOutput (fromIntegral (length declared)) (TxOut o)]
              L.SNothing -> []

-- | Every datum and script preimage a block carries.
--
-- Four sources, deliberately a superset of kupo's two. kupo collects only
-- witness-set datums and witness-set/auxiliary scripts; we also take the /inline/
-- datum and the /reference script/ straight off each output, where the body is
-- already in hand for free. On preview to slot 4,000,000 kupo ends up with bodies
-- for 182,228 of the 182,370 distinct datum hashes its outputs reference — the
-- inline sources are what close that gap.
--
-- The caller decides whether to write these: see the relevance gate in
-- "Cardano.Sieve.Node.Insert", which mirrors kupo's — store a block's preimages
-- only if that block produced a tracked output or spent a tracked input, so a
-- narrow selector does not drag in the whole chain's datums.
preimagesInBlock :: BlockInMode -> Preimages
preimagesInBlock (BlockInMode _ block) = foldMap txPreimages (getBlockTxs block)

-- | The preimages of one transaction. Every era is enumerated (no wildcard) so a
-- future era must be handled explicitly rather than silently yielding nothing.
-- Note on shape: the era-specific lenses ('datsTxWitsL' needs @AlonzoEraTxWits@,
-- 'datumTxOutF' @AlonzoEraTxOut@, 'referenceScriptTxOutL' @BabbageEraTxOut@) are
-- applied INSIDE the concrete @case sbe of@ branches. 'shelleyBasedEraConstraints'
-- only brings the era-generic constraints into scope, so hoisting those reads into
-- a @let@ fails to typecheck — the same trap as building the collateral output via
-- a polymorphic helper. Only the era-agnostic post-processing is factored out.
txPreimages :: Tx era -> Preimages
txPreimages (ShelleyTx sbe ledgerTx) =
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

        -- Scripts, per era: the witness set throughout, plus output reference
        -- scripts from Babbage. Each branch inlines its own comprehension rather
        -- than sharing a helper: the script type differs by era (a bare native
        -- script before Alonzo, 'AlonzoScript' after), and @GADTs@ implies
        -- @MonoLocalBinds@, so one @let@-bound helper would be pinned to a single
        -- era's type. Matching @sbe@ also refines @era@ to a concrete era, which is
        -- what brings @AlonzoEraScript@ into scope for 'plutusScriptLanguage'.
        --
        -- Every stored script is prefixed with its one-byte LANGUAGE TAG — 0 for a
        -- native script, then 1/2/3/4 for PlutusV1..V4. This is not decoration: a
        -- Cardano script hash is @blake2b224 (tag <> scriptBytes)@, so the tag is
        -- part of the hash preimage. Without it the stored blob does not hash to
        -- the key it is filed under, and a caller cannot verify what it was given.
        -- It is also the only thing distinguishing byte-identical scripts under
        -- different Plutus versions — they are genuinely different scripts with
        -- different hashes. Verified: @blake2b224 (01 <> bytes)@ reproduces the
        -- @script_hash@ exactly, and dropping the tag does not.
        scriptRowsOf tag ss =
          [ (serialiseToRawBytes (fromShelleyScriptHash sh), BS.cons tag (originalBytes s))
          | (sh, s) <- ss
          ]
        plutusTag s = case s of
          NativeScript _ -> 0
          PlutusScript ps -> fromIntegral (1 + fromEnum (plutusScriptLanguage ps))
        scripts = case sbe of
          ShelleyBasedEraShelley -> []
          ShelleyBasedEraAllegra ->
            scriptRowsOf 0 (Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL))
          ShelleyBasedEraMary ->
            scriptRowsOf 0 (Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL))
          ShelleyBasedEraAlonzo ->
            [ (serialiseToRawBytes (fromShelleyScriptHash sh), BS.cons (plutusTag s) (originalBytes s))
            | (sh, s) <- Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL)
            ]
          ShelleyBasedEraBabbage ->
            [ (serialiseToRawBytes (fromShelleyScriptHash sh), BS.cons (plutusTag s) (originalBytes s))
            | (sh, s) <- Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL)
            ]
              <> [ ( serialiseToRawBytes (fromShelleyScriptHash (L.hashScript s))
                   , BS.cons (plutusTag s) (originalBytes s)
                   )
                 | o <- outs
                 , L.SJust s <- [o ^. L.referenceScriptTxOutL]
                 ]
          ShelleyBasedEraConway ->
            [ (serialiseToRawBytes (fromShelleyScriptHash sh), BS.cons (plutusTag s) (originalBytes s))
            | (sh, s) <- Map.toList (ledgerTx ^. L.witsTxL . L.scriptTxWitsL)
            ]
              <> [ ( serialiseToRawBytes (fromShelleyScriptHash (L.hashScript s))
                   , BS.cons (plutusTag s) (originalBytes s)
                   )
                 | o <- outs
                 , L.SJust s <- [o ^. L.referenceScriptTxOutL]
                 ]
     in Preimages datums scripts

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
-- index @n@ is exactly the input at @siInputIndex = n@ — the reason sieve's
-- @input_index@ already agreed with kupo's.
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

-- | Serialise a selected output to the bytes the schema stores.
toStored :: DecodedOutput -> StoredOutput
toStored o =
  StoredOutput
    { soOutputRef = encodeOutputRef (doOutputRef o)
    , soTransactionIndex = fromIntegral (doTransactionIndex o)
    , soAddress = serialiseToRawBytes (doAddress o)
    , soPayCred = paymentHash (doAddress o)
    , soDelegCred = delegationHash (doAddress o)
    , soValue = encodeValue (doValue o)
    , soDatumHash = snd <$> doDatum o
    , soDatumType = fst <$> doDatum o
    , soReferenceScriptHash = doReferenceScriptHash o
    , soAssets = assetsOf (doValue o)
    }

-- | Encode an output reference as the transaction id bytes followed by the
-- output index as a big-endian 'Word64'. Fixed-width and order-preserving.
encodeOutputRef :: TxIn -> ByteString
encodeOutputRef (TxIn txid (TxIx ix)) =
  serialiseToRawBytes txid
    <> LBS.toStrict (toLazyByteString (word64BE (fromIntegral ix)))

-- | The distinct (policy id, asset name) pairs of the positive-quantity assets
-- in a value (ada excluded). Both are raw bytes; the asset name may be empty.
assetsOf :: Value -> [(ByteString, ByteString)]
assetsOf v =
  Set.toList
    ( Set.fromList
        [(serialiseToRawBytes pid, serialiseToRawBytes name) | (AssetId pid name, q) <- toList v, q > 0]
    )
