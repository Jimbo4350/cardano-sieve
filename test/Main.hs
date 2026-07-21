{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for @cardano-sieve@.
--
-- Phase 2 covers the pure matcher: for each 'Selector' constructor, an output
-- built from known credentials/assets is checked against 'satisfies'. Fixtures
-- are constructed via @cardano-api@ (so we control the exact 28-byte hashes)
-- rather than hard-coded, and the tricky cases are exercised deliberately:
-- key-vs-script agnostic credential matching, base-vs-enterprise-vs-Byron
-- delegation, and the bootstrap wildcard.
module Main (main) where

import Cardano.Api
  ( AddressAny
  , AsType (AsAddressAny, AsAssetName, AsHash, AsPaymentKey, AsScriptHash, AsStakeKey, AsTxId)
  , AssetId (AssetId)
  , AssetName
  , Hash
  , NetworkId (Mainnet)
  , PaymentCredential (PaymentCredentialByKey, PaymentCredentialByScript)
  , PaymentKey
  , PolicyId (PolicyId)
  , ScriptHash
  , SerialiseAsRawBytes
  , StakeAddressReference (NoStakeAddress, StakeAddressByValue)
  , StakeCredential (StakeCredentialByKey)
  , StakeKey
  , TxId
  , TxIn (TxIn)
  , TxIx (TxIx)
  , Value
  , deserialiseAddress
  , deserialiseFromRawBytes
  , makeShelleyAddress
  , toAddressAny
  )

import Cardano.Sieve.Satisfies (OutputContext (..), satisfies)
import Cardano.Sieve.Selector
  ( BootstrapFilter (IncludeBootstrap, OnlyShelley)
  , CredentialHash
  , Selector (..)
  , credentialHashFromBytes
  )

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Exts (fromList)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "cardano-sieve"
    [ selectorTests
    , matcherTests
    ]

-- ----------------------------------------------------------------------------
-- Fixtures
-- ----------------------------------------------------------------------------

-- | Deserialise from raw bytes, exploding on failure (fine in test fixtures).
raw :: SerialiseAsRawBytes a => AsType a -> ByteString -> a
raw t = either (error . show) id . deserialiseFromRawBytes t

-- Distinct 28-byte credential hashes and a 32-byte transaction id.
payBytes, stakeBytes, otherBytes, policyBytes :: ByteString
payBytes = BS.replicate 28 11
stakeBytes = BS.replicate 28 22
otherBytes = BS.replicate 28 99
policyBytes = BS.replicate 28 55

payKeyHash :: Hash PaymentKey
payKeyHash = raw (AsHash AsPaymentKey) payBytes

-- | A /script/ hash sharing the same 28 bytes as 'payKeyHash', to prove the
-- match is agnostic to the key-vs-script kind.
payScriptHash :: ScriptHash
payScriptHash = raw AsScriptHash payBytes

stakeKeyHash :: Hash StakeKey
stakeKeyHash = raw (AsHash AsStakeKey) stakeBytes

-- | Base address: payment key hash + stake key hash.
baseAddr :: AddressAny
baseAddr =
  toAddressAny $
    makeShelleyAddress
      Mainnet
      (PaymentCredentialByKey payKeyHash)
      (StakeAddressByValue (StakeCredentialByKey stakeKeyHash))

-- | Enterprise address: payment part only, no delegation.
enterpriseAddr :: AddressAny
enterpriseAddr =
  toAddressAny $
    makeShelleyAddress Mainnet (PaymentCredentialByKey payKeyHash) NoStakeAddress

-- | Enterprise address whose payment part is a /script/ credential with the
-- same hash bytes as 'baseAddr'/'enterpriseAddr'.
scriptPayAddr :: AddressAny
scriptPayAddr =
  toAddressAny $
    makeShelleyAddress Mainnet (PaymentCredentialByScript payScriptHash) NoStakeAddress

-- | A real mainnet Byron (bootstrap) address; constructing one programmatically
-- is awkward, so we parse a known-valid one.
byronAddr :: AddressAny
byronAddr =
  fromMaybe (error "byron fixture failed to parse") $
    deserialiseAddress AsAddressAny byronText

byronText :: Text
byronText = "Ae2tdPwUPEZ4YjgvykNpoFeYUxoyhNj2kg8KfKWN2FizsSpLUPv68MpTVDo"

payCH, stakeCH, otherCH :: CredentialHash
payCH = mkCH payBytes
stakeCH = mkCH stakeBytes
otherCH = mkCH otherBytes

mkCH :: ByteString -> CredentialHash
mkCH bs = fromMaybe (error "bad credential-hash fixture") (credentialHashFromBytes bs)

txid, otherTxid :: TxId
txid = raw AsTxId (BS.replicate 32 33)
otherTxid = raw AsTxId (BS.replicate 32 44)

outRef0 :: TxIn
outRef0 = TxIn txid (TxIx 0)

policyId :: PolicyId
policyId = PolicyId (raw AsScriptHash policyBytes)

assetName, otherAssetName :: AssetName
assetName = raw AsAssetName (BS.pack [84, 79, 75]) -- "TOK"
otherAssetName = raw AsAssetName (BS.pack [88]) -- "X"

assetValue :: Value
assetValue = fromList [(AssetId policyId assetName, 5)]

-- | An output context at a given address, empty value and no metadata tags.
ctxAt :: AddressAny -> OutputContext
ctxAt addr =
  OutputContext
    { ocOutputRef = outRef0
    , ocAddress = addr
    , ocValue = mempty
    , ocMetadataTags = Set.empty
    }

-- ----------------------------------------------------------------------------
-- Tests
-- ----------------------------------------------------------------------------

selectorTests :: TestTree
selectorTests =
  testGroup
    "Selector"
    [ testCase "SelectAll equality distinguishes the bootstrap filter" $ do
        (SelectAll IncludeBootstrap == SelectAll IncludeBootstrap) @?= True
        (SelectAll IncludeBootstrap == SelectAll OnlyShelley) @?= False
    ]

matcherTests :: TestTree
matcherTests =
  testGroup
    "Satisfies.satisfies"
    [ testCase "SelectExact matches only the exact address" $ do
        satisfies (ctxAt baseAddr) (SelectExact baseAddr) @?= True
        satisfies (ctxAt baseAddr) (SelectExact enterpriseAddr) @?= False
    , testCase "SelectPayment matches base, enterprise, and script (key/script agnostic)" $ do
        satisfies (ctxAt baseAddr) (SelectPayment payCH) @?= True
        satisfies (ctxAt enterpriseAddr) (SelectPayment payCH) @?= True
        satisfies (ctxAt scriptPayAddr) (SelectPayment payCH) @?= True
        satisfies (ctxAt baseAddr) (SelectPayment otherCH) @?= False
        satisfies (ctxAt byronAddr) (SelectPayment payCH) @?= False
    , testCase "SelectDelegation matches only base addresses' stake part" $ do
        satisfies (ctxAt baseAddr) (SelectDelegation stakeCH) @?= True
        satisfies (ctxAt baseAddr) (SelectDelegation otherCH) @?= False
        satisfies (ctxAt enterpriseAddr) (SelectDelegation stakeCH) @?= False
        satisfies (ctxAt byronAddr) (SelectDelegation stakeCH) @?= False
    , testCase "SelectPaymentAndDelegation needs both parts to match" $ do
        satisfies (ctxAt baseAddr) (SelectPaymentAndDelegation payCH stakeCH) @?= True
        satisfies (ctxAt baseAddr) (SelectPaymentAndDelegation payCH otherCH) @?= False
        satisfies (ctxAt baseAddr) (SelectPaymentAndDelegation otherCH stakeCH) @?= False
        satisfies (ctxAt enterpriseAddr) (SelectPaymentAndDelegation payCH stakeCH) @?= False
    , testCase "SelectTransactionId matches the producing transaction" $ do
        satisfies (ctxAt baseAddr) (SelectTransactionId txid) @?= True
        satisfies (ctxAt baseAddr) (SelectTransactionId otherTxid) @?= False
    , testCase "SelectOutputReference matches the exact output" $ do
        satisfies (ctxAt baseAddr) (SelectOutputReference outRef0) @?= True
        satisfies (ctxAt baseAddr) (SelectOutputReference (TxIn txid (TxIx 1))) @?= False
    , testCase "SelectPolicyId matches an output carrying the policy" $ do
        satisfies (ctxAt enterpriseAddr){ocValue = assetValue} (SelectPolicyId policyId) @?= True
        satisfies (ctxAt enterpriseAddr) (SelectPolicyId policyId) @?= False
    , testCase "SelectAssetId matches an output carrying the exact asset" $ do
        satisfies (ctxAt enterpriseAddr){ocValue = assetValue} (SelectAssetId policyId assetName) @?= True
        satisfies (ctxAt enterpriseAddr){ocValue = assetValue} (SelectAssetId policyId otherAssetName)
          @?= False
        satisfies (ctxAt enterpriseAddr) (SelectAssetId policyId assetName) @?= False
    , testCase "SelectMetadataTag matches a tag on the producing transaction" $ do
        let ctxM = (ctxAt enterpriseAddr){ocMetadataTags = Set.fromList [674, 721]}
        satisfies ctxM (SelectMetadataTag 674) @?= True
        satisfies ctxM (SelectMetadataTag 999) @?= False
        satisfies (ctxAt enterpriseAddr) (SelectMetadataTag 674) @?= False
    , testCase "SelectAll IncludeBootstrap matches every address (Byron included)" $ do
        satisfies (ctxAt baseAddr) (SelectAll IncludeBootstrap) @?= True
        satisfies (ctxAt byronAddr) (SelectAll IncludeBootstrap) @?= True
    , testCase "SelectAll OnlyShelley excludes Byron addresses" $ do
        satisfies (ctxAt baseAddr) (SelectAll OnlyShelley) @?= True
        satisfies (ctxAt byronAddr) (SelectAll OnlyShelley) @?= False
    ]
