{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The 'Selector' ADT — the semantic core of the sieve — together with its
-- textual surface syntax ('selectorFromText' / 'selectorToText') and the pure
-- matcher ('satisfies').
--
-- The type is the source of truth; the codec is one way to construct and render
-- it. They live together (as kupo pairs its @Pattern@ type with
-- @patternFromText@) so the syntax and the constructors it targets stay in
-- lock-step.
--
-- Most payloads reuse @cardano-api@ types (addresses, policy/asset ids,
-- transaction references) so matching against decoded blocks needs no
-- conversion. The payment/delegation parts instead use 'CredentialHash' — a
-- bare 28-byte hash — so that matching is agnostic to whether a credential is a
-- key or a script.
--
-- == Surface syntax
--
-- Parsing is /dispatch-then-decode/ rather than kupo's try-everything: we branch
-- once on the structural delimiter present in the input, then run the single
-- decoder that delimiter implies. That lets a failure name the shape it was
-- decoding ('SelectorParseError') instead of collapsing every malformed input to
-- a bare 'Nothing'. The delimiters are mutually exclusive across the syntax (no
-- address encoding contains @/ \@ . { }@), so the dispatch is unambiguous:
--
--   * @*@                            — every output (Byron included)
--   * @{n}@                          — metadata tag @n@
--   * @payment/delegation@           — address parts (@*@ in a slot leaves it free)
--   * @index\@txid@ / @*\@txid@      — one output / a whole transaction
--   * @policyId.name@ / @policyId.*@ — an asset / a whole policy
--   * anything else                  — a whole address (bech32/base58/base16)
--
-- 'selectorToText' is the inverse; @'selectorFromText' . 'selectorToText'@ is the
-- identity on 'Selector' (the round-trip property; the rendered form is
-- canonical, so a bech32 credential input re-renders as hex).
--
-- == Matching
--
-- 'satisfies' answers whether a single output satisfies a selector, given an
-- 'OutputContext' — the minimal cut of a decoded output (in its transaction
-- context) the matcher depends on. An output is indexed when it satisfies
-- /any/ configured selector, so the ingest stage folds it:
-- @any ('satisfies' ctx) selectors@. Matching is pure — no chain decode, no
-- persistence — so it stays unit-testable in isolation, and the decode stage
-- ("Cardano.Sieve.Node.Decode") is what builds an 'OutputContext' per output.
module Cardano.Sieve.Selector
  ( Selector (..)
  , BootstrapFilter (..)
  , CredentialHash
  , credentialHashFromBytes
  , credentialHashToBytes
  , selectorFromText
  , selectorToText
  , SelectorParseError (..)
  , OutputContext (..)
  , satisfies
  , paymentHash
  , delegationHash
  )
where

import Cardano.Api
  ( Address (ShelleyAddress)
  , AddressAny (AddressByron, AddressShelley)
  , AsType
    ( AsAddressAny
    , AsPaymentKey
    , AsStakeAddress
    , AsVerificationKey
    )
  , AssetId (AdaAssetId, AssetId)
  , AssetName
  , PaymentCredential (PaymentCredentialByKey, PaymentCredentialByScript)
  , PolicyId
  , StakeAddressReference (NoStakeAddress, StakeAddressByPointer, StakeAddressByValue)
  , StakeCredential (StakeCredentialByKey, StakeCredentialByScript)
  , TxId
  , TxIn (TxIn)
  , TxIx (TxIx)
  , Value
  , deserialiseAddress
  , deserialiseFromRawBytes
  , deserialiseFromRawBytesHex
  , fromShelleyPaymentCredential
  , fromShelleyStakeReference
  , selectAsset
  , serialiseAddress
  , serialiseToRawBytes
  , serialiseToRawBytesHexText
  , stakeAddressCredential
  , verificationKeyHash
  )

import Codec.Binary.Bech32 qualified as Bech32
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Text.Read qualified as T (decimal)
import Data.Word (Word64)
import GHC.Exts (toList)

-- | The 28-byte hash of an address credential, /agnostic/ to whether that
-- credential is a key hash or a script hash.
--
-- Matching compares only these bytes: the address header bit that distinguishes
-- a key credential from a script credential is deliberately ignored, so a bare
-- hash need not be classified as one or the other. Construct with
-- 'credentialHashFromBytes', which enforces the length.
newtype CredentialHash = CredentialHash ByteString
  deriving (Eq, Ord, Show)

-- | Build a 'CredentialHash' from raw bytes, or 'Nothing' if they are not
-- exactly 28 bytes long (the width of a blake2b-224 credential hash).
credentialHashFromBytes :: ByteString -> Maybe CredentialHash
credentialHashFromBytes bytes
  | BS.length bytes == 28 = Just (CredentialHash bytes)
  | otherwise = Nothing

-- | The raw 28 bytes underlying a 'CredentialHash'.
credentialHashToBytes :: CredentialHash -> ByteString
credentialHashToBytes (CredentialHash bytes) = bytes

-- | Whether a wildcard match includes legacy Byron (bootstrap) addresses.
--
-- Byron addresses have no delegation (stake) part, so they cannot be selected by
-- a match on address parts. 'IncludeBootstrap' widens a wildcard to cover every
-- address, Byron included; 'OnlyShelley' restricts it to Shelley-era addresses.
data BootstrapFilter
  = -- | Include Byron (bootstrap) addresses.
    IncludeBootstrap
  | -- | Shelley-era addresses only.
    OnlyShelley
  deriving (Eq, Show)

-- | A single indexing criterion: one rule for deciding whether a transaction
-- output should be kept.
--
-- Each constructor names one /dimension/ an output (or its transaction) can be
-- selected by — the whole address, one part of the address, the transaction it
-- belongs to, the native assets it carries, or a metadata tag on its
-- transaction. During indexing every decoded output is tested against the
-- configured 'Selector's and kept if /any/ of them matches; the configured set
-- of 'Selector's is therefore what forms the "sieve" the chain is run through.
data Selector
  = -- | Wildcard: match every output, subject to the 'BootstrapFilter'.
    SelectAll BootstrapFilter
  | -- | Match outputs sent to exactly this address — network discriminant and
    -- both address parts included.
    SelectExact AddressAny
  | -- | Match outputs whose address has this payment credential hash, whatever
    -- its delegation part (a key and a script credential with the same hash
    -- both match).
    SelectPayment CredentialHash
  | -- | Match outputs whose address has this delegation (stake) credential
    -- hash, whatever its payment part.
    SelectDelegation CredentialHash
  | -- | Match outputs whose address has both this payment credential hash and
    -- this delegation credential hash.
    SelectPaymentAndDelegation CredentialHash CredentialHash
  | -- | Match every output produced by this transaction.
    SelectTransactionId TxId
  | -- | Match one specific output, identified by its full output reference
    -- (transaction id + output index).
    SelectOutputReference TxIn
  | -- | Match outputs carrying a positive quantity of any asset minted under
    -- this policy.
    SelectPolicyId PolicyId
  | -- | Match outputs carrying a positive quantity of this exact asset
    -- (policy id + asset name).
    SelectAssetId PolicyId AssetName
  | -- | Match every output of any transaction carrying this top-level metadata
    -- tag (label).
    --
    -- Ingest-only, and the odd one out: unlike every other constructor it is
    -- /not/ queryable after the fact (it only decides what to index), and it is
    -- the single selector that requires the decode stage to surface each
    -- transaction's metadata to the matcher.
    SelectMetadataTag Word64
  deriving (Eq, Show)

-- | Why a piece of text is not a valid 'Selector'. Because parsing dispatches on
-- the delimiter first, the error names the shape that /was/ attempted rather
-- than being a catch-all — @not.hex@ (a @.@, so read as an asset id) fails as
-- 'BadPolicyId', not a generic error.
data SelectorParseError
  = -- | Carried delimiter(s) that formed no valid shape (e.g. more than one
    -- @.@), so no specific decoder applied. A bare token that merely fails to
    -- decode as an address is a 'BadAddress', not this.
    UnrecognisedShape Text
  | BadAddress Text
  | BadCredential Text
  | BadTransactionId Text
  | BadOutputIndex Text
  | BadPolicyId Text
  | BadAssetName Text
  | BadMetadataTag Text
  deriving (Eq, Show)

-- | Parse a selector from its textual form. See the module header for the
-- syntax.
selectorFromText :: Text -> Either SelectorParseError Selector
selectorFromText txt
  | txt == wildcard = Right (SelectAll IncludeBootstrap)
  | Just body <- braced txt = parseMetadataTag body
  | Just (p, d) <- splitOn2 '/' txt = parsePaymentDelegation p d
  | Just (i, t) <- splitOn2 '@' txt = parseOutputRef i t
  | Just (p, n) <- splitOn2 '.' txt = parseAssetId p n
  | otherwise = parseWholeAddress txt

-- | @payment/delegation@. A @*@ in either slot leaves that address part free.
parsePaymentDelegation :: Text -> Text -> Either SelectorParseError Selector
parsePaymentDelegation payment delegation
  | payment == wildcard && delegation == wildcard =
      Right (SelectAll OnlyShelley)
  | payment == wildcard =
      SelectDelegation <$> parseCredential delegation
  | delegation == wildcard =
      SelectPayment <$> parseCredential payment
  | otherwise =
      SelectPaymentAndDelegation
        <$> parseCredential payment
        <*> parseCredential delegation

-- | @index\@txid@ (one output) or @*\@txid@ (a whole transaction).
parseOutputRef :: Text -> Text -> Either SelectorParseError Selector
parseOutputRef ix txid
  | ix == wildcard = SelectTransactionId <$> parseTxId txid
  | otherwise = SelectOutputReference <$> (TxIn <$> parseTxId txid <*> parseTxIx ix)

-- | @policyId.name@ (an asset) or @policyId.*@ (any asset of a policy).
parseAssetId :: Text -> Text -> Either SelectorParseError Selector
parseAssetId policyId name
  | name == wildcard = SelectPolicyId <$> parsePolicyId policyId
  | otherwise = SelectAssetId <$> parsePolicyId policyId <*> parseAssetName name

-- | A whole address: bech32 @addr@/@addr_test@ or base58 (Byron) → 'SelectExact';
-- bech32 @stake@/@stake_test@ → 'SelectDelegation' (matching on the stake part,
-- as kupo does); base16 → 'SelectExact'.
parseWholeAddress :: Text -> Either SelectorParseError Selector
parseWholeAddress txt =
  maybe (Left err) Right (asAddress <|> asStakeAddress <|> asHexAddress)
 where
  err = if isDelimited txt then UnrecognisedShape txt else BadAddress txt
  asAddress = SelectExact <$> deserialiseAddress AsAddressAny txt
  asHexAddress = SelectExact <$> hush (deserialiseFromRawBytesHex @AddressAny (encodeUtf8 txt))
  asStakeAddress = do
    sa <- deserialiseAddress AsStakeAddress txt
    SelectDelegation <$> credentialHashFromBytes (stakeCredentialBytes (stakeAddressCredential sa))

parseMetadataTag :: Text -> Either SelectorParseError Selector
parseMetadataTag body =
  maybe (Left (BadMetadataTag body)) (Right . SelectMetadataTag) (readDecimal body)

-- | A credential: bare 28-byte hash used as-is, or a 32-byte verification key
-- hashed (blake2b-224) down to one. Accepted as base16, or as bech32 with an
-- HRP consistent with the length (@vk@/@addr_vk@/@stake_vk@ for keys,
-- @vkh@/@addr_vkh@/@stake_vkh@/@script@ for hashes). The key-vs-script kind is
-- discarded — only the 28 hash bytes matter for matching.
parseCredential :: Text -> Either SelectorParseError CredentialHash
parseCredential txt =
  maybe (Left (BadCredential txt)) Right (fromHex <|> fromBech32)
 where
  fromHex = credentialFromBytes =<< hush (Base16.decode (encodeUtf8 txt))
  fromBech32 = case Bech32.decodeLenient txt of
    Left _ -> Nothing
    Right (hrp, dataPart) -> do
      bytes <- Bech32.dataPartToBytes dataPart
      credentialFromBech32 (Bech32.humanReadablePartToText hrp) bytes

-- | Interpret raw credential bytes by length: 28 → a hash used directly, 32 → a
-- verification key hashed to its 28-byte hash. Any other length is rejected.
credentialFromBytes :: ByteString -> Maybe CredentialHash
credentialFromBytes bytes
  | BS.length bytes == 28 = credentialHashFromBytes bytes
  | BS.length bytes == 32 = hashVerificationKey bytes
  | otherwise = Nothing

-- | As 'credentialFromBytes', but with the bech32 HRP tying down which lengths
-- are legal: keys must carry a key HRP, hashes a hash HRP.
credentialFromBech32 :: Text -> ByteString -> Maybe CredentialHash
credentialFromBech32 hrp bytes
  | BS.length bytes == 32, hrp `elem` keyHrps = hashVerificationKey bytes
  | BS.length bytes == 28, hrp `elem` hashHrps = credentialHashFromBytes bytes
  | otherwise = Nothing
 where
  keyHrps = ["vk", "addr_vk", "stake_vk"]
  hashHrps = ["vkh", "addr_vkh", "stake_vkh", "script"]

-- | Hash a 32-byte ed25519 verification key to its 28-byte credential hash,
-- reusing cardano-api's key machinery rather than hashing bytes ourselves.
hashVerificationKey :: ByteString -> Maybe CredentialHash
hashVerificationKey bytes = do
  vk <- hush (deserialiseFromRawBytes (AsVerificationKey AsPaymentKey) bytes)
  credentialHashFromBytes (serialiseToRawBytes (verificationKeyHash vk))

-- | The 28 raw hash bytes of a stake credential, discarding the key/script kind.
stakeCredentialBytes :: StakeCredential -> ByteString
stakeCredentialBytes = \case
  StakeCredentialByKey keyHash -> serialiseToRawBytes keyHash
  StakeCredentialByScript scriptHash -> serialiseToRawBytes scriptHash

parseTxId :: Text -> Either SelectorParseError TxId
parseTxId txt =
  maybe
    (Left (BadTransactionId txt))
    Right
    (hush (deserialiseFromRawBytesHex @TxId (encodeUtf8 txt)))

parsePolicyId :: Text -> Either SelectorParseError PolicyId
parsePolicyId txt =
  maybe (Left (BadPolicyId txt)) Right (hush (deserialiseFromRawBytesHex @PolicyId (encodeUtf8 txt)))

parseAssetName :: Text -> Either SelectorParseError AssetName
parseAssetName txt =
  maybe
    (Left (BadAssetName txt))
    Right
    (hush (deserialiseFromRawBytesHex @AssetName (encodeUtf8 txt)))

parseTxIx :: Text -> Either SelectorParseError TxIx
parseTxIx txt = maybe (Left (BadOutputIndex txt)) (Right . TxIx) (readDecimal txt)

-- | Render a selector back to its canonical textual form (the inverse of
-- 'selectorFromText').
selectorToText :: Selector -> Text
selectorToText = \case
  SelectAll IncludeBootstrap -> wildcard
  SelectAll OnlyShelley -> wildcard <> "/" <> wildcard
  SelectExact addr -> serialiseAddress addr
  SelectPayment ch -> credentialToHex ch <> "/" <> wildcard
  SelectDelegation ch -> wildcard <> "/" <> credentialToHex ch
  SelectPaymentAndDelegation p d -> credentialToHex p <> "/" <> credentialToHex d
  SelectTransactionId txid -> wildcard <> "@" <> serialiseToRawBytesHexText txid
  SelectOutputReference (TxIn txid (TxIx ix)) ->
    T.pack (show ix) <> "@" <> serialiseToRawBytesHexText txid
  SelectPolicyId pid -> serialiseToRawBytesHexText pid <> "." <> wildcard
  SelectAssetId pid name -> serialiseToRawBytesHexText pid <> "." <> serialiseToRawBytesHexText name
  SelectMetadataTag tag -> "{" <> T.pack (show tag) <> "}"

credentialToHex :: CredentialHash -> Text
credentialToHex = decodeLatin1 . Base16.encode . credentialHashToBytes

-- * Matching

-- | Everything the matcher needs about one transaction output, in the context
-- of the transaction that produced it. Every 'Selector' constructor is served
-- by exactly one field. The decode stage builds one of these per output.
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

-- | Does this output, in its transaction context, satisfy the selector? An
-- output is indexed when it satisfies /any/ configured selector, so callers
-- fold this over the selector set: @any ('satisfies' ctx) selectors@.
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

-- * Small helpers

wildcard :: Text
wildcard = "*"

-- | The inner text of a @{...}@ wrapper, or 'Nothing' if not so wrapped.
braced :: Text -> Maybe Text
braced txt = T.stripPrefix "{" txt >>= T.stripSuffix "}"

-- | Split on the single occurrence of a single-character delimiter, succeeding
-- only when it splits the text into exactly two parts.
splitOn2 :: Char -> Text -> Maybe (Text, Text)
splitOn2 c txt = case T.splitOn (T.singleton c) txt of
  [a, b] -> Just (a, b)
  _ -> Nothing

-- | Whether the text carries any structural delimiter — used only to choose
-- between 'UnrecognisedShape' and 'BadAddress' for a bare string.
isDelimited :: Text -> Bool
isDelimited txt = any (`T.isInfixOf` txt) ["/", "@", ".", "{", "}"]

-- | Parse a whole 'Text' as a non-negative decimal, rejecting trailing input.
readDecimal :: Integral a => Text -> Maybe a
readDecimal txt = case T.decimal txt of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
