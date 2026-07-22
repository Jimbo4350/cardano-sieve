{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | The pure matcher: does a decoded output satisfy a 'Selector'?
--
-- This is the "match" stage in isolation — no chain decoding, no persistence —
-- so it is fully unit-testable. 'OutputContext' is the minimal cut of a decoded
-- output (in its transaction context) that the matcher depends on; the decode
-- stage is responsible for building one per output.
--
-- An output is indexed when it satisfies /any/ configured selector, so callers
-- fold 'satisfies' over the selector set: @any ('satisfies' ctx) selectors@.
--
-- Address parts are taken from the ledger's structured view of the address
-- ('Cardano.Api.fromShelleyPaymentCredential' /
-- 'Cardano.Api.fromShelleyStakeReference') rather than by slicing raw bytes, so
-- the CIP-19 layout is the library's problem, not ours. The credential is then
-- reduced to its 28 raw hash bytes for comparison, which is what makes a match
-- agnostic to whether the on-chain credential is a key or a script (see
-- @CredentialHash@ in "Cardano.Sieve.Selector").
module Cardano.Sieve.Satisfies
  ( OutputContext (..)
  , satisfies
  , paymentHash
  , delegationHash
  )
where

import Cardano.Api
  ( Address (ShelleyAddress)
  , AddressAny (AddressByron, AddressShelley)
  , AssetId (AdaAssetId, AssetId)
  , PaymentCredential (PaymentCredentialByKey, PaymentCredentialByScript)
  , PolicyId
  , StakeAddressReference (NoStakeAddress, StakeAddressByPointer, StakeAddressByValue)
  , StakeCredential (StakeCredentialByKey, StakeCredentialByScript)
  , TxId
  , TxIn (TxIn)
  , Value
  , fromShelleyPaymentCredential
  , fromShelleyStakeReference
  , selectAsset
  , serialiseToRawBytes
  )

import Cardano.Sieve.Selector
  ( BootstrapFilter (IncludeBootstrap, OnlyShelley)
  , Selector (..)
  , credentialHashToBytes
  )

import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Exts (toList)

-- | Everything the matcher needs about one transaction output, in the context of
-- the transaction that produced it. Every 'Selector' constructor is served by
-- exactly one field.
data OutputContext = OutputContext
  { ocOutputRef :: TxIn
  -- ^ Full output reference (transaction id + index); the id also serves
  -- 'SelectTransactionId'.
  , ocAddress :: AddressAny
  -- ^ The output's address.
  , ocValue :: Value
  -- ^ The output's value (ada + native assets).
  , ocMetadataTags :: Set Word64
  -- ^ Top-level metadata labels on the producing transaction (empty if none).
  }

-- | Does this output, in its transaction context, satisfy the selector?
satisfies :: OutputContext -> Selector -> Bool
satisfies ctx = \case
  SelectAll IncludeBootstrap ->
    True
  SelectAll OnlyShelley ->
    isShelley (ocAddress ctx)
  SelectExact addr ->
    ocAddress ctx == addr
  SelectPayment ch ->
    paymentHash (ocAddress ctx) == Just (credentialHashToBytes ch)
  SelectDelegation ch ->
    delegationHash (ocAddress ctx) == Just (credentialHashToBytes ch)
  SelectPaymentAndDelegation pc dc ->
    paymentHash (ocAddress ctx) == Just (credentialHashToBytes pc)
      && delegationHash (ocAddress ctx) == Just (credentialHashToBytes dc)
  SelectTransactionId txid ->
    txIdOf (ocOutputRef ctx) == txid
  SelectOutputReference ref ->
    ocOutputRef ctx == ref
  SelectPolicyId pid ->
    any (\(aid, q) -> q > 0 && assetPolicyId aid == Just pid) (toList (ocValue ctx))
  SelectAssetId pid name ->
    selectAsset (ocValue ctx) (AssetId pid name) > 0
  SelectMetadataTag tag ->
    tag `Set.member` ocMetadataTags ctx

-- | The transaction id of an output reference.
txIdOf :: TxIn -> TxId
txIdOf (TxIn txid _) = txid

-- | Whether an address is a Shelley-era address (Byron/bootstrap excluded).
isShelley :: AddressAny -> Bool
isShelley = \case
  AddressShelley _ -> True
  AddressByron _ -> False

-- | The policy under which an asset is minted, or 'Nothing' for ada.
assetPolicyId :: AssetId -> Maybe PolicyId
assetPolicyId = \case
  AdaAssetId -> Nothing
  AssetId pid _ -> Just pid

-- | The 28-byte payment credential hash of an address, or 'Nothing' for Byron.
-- The key-vs-script kind is discarded: a key credential and a script credential
-- with the same hash yield the same bytes.
paymentHash :: AddressAny -> Maybe ByteString
paymentHash = \case
  AddressByron _ ->
    Nothing
  AddressShelley (ShelleyAddress _ pc _) ->
    Just (paymentCredentialBytes (fromShelleyPaymentCredential pc))

-- | The 28-byte delegation (stake) credential hash of an address. Only base
-- addresses carry one by value; pointer, enterprise, and Byron addresses have
-- none and return 'Nothing'.
delegationHash :: AddressAny -> Maybe ByteString
delegationHash = \case
  AddressByron _ ->
    Nothing
  AddressShelley (ShelleyAddress _ _ sr) ->
    case fromShelleyStakeReference sr of
      StakeAddressByValue sc -> Just (stakeCredentialBytes sc)
      StakeAddressByPointer _ -> Nothing
      NoStakeAddress -> Nothing

-- | The 28 raw hash bytes of a payment credential, discarding the key/script
-- kind so the match stays credential-type-agnostic.
paymentCredentialBytes :: PaymentCredential -> ByteString
paymentCredentialBytes = \case
  PaymentCredentialByKey keyHash -> serialiseToRawBytes keyHash
  PaymentCredentialByScript scriptHash -> serialiseToRawBytes scriptHash

-- | The 28 raw hash bytes of a stake credential, discarding the key/script kind.
stakeCredentialBytes :: StakeCredential -> ByteString
stakeCredentialBytes = \case
  StakeCredentialByKey keyHash -> serialiseToRawBytes keyHash
  StakeCredentialByScript scriptHash -> serialiseToRawBytes scriptHash
