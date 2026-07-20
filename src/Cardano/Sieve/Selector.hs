{-# LANGUAGE ImportQualifiedPost #-}

-- | The 'Selector' ADT — the semantic core of the sieve.
--
-- This module defines only the type, which is the source of truth. How these
-- values are specified by the user (the CLI surface for selectors) is a
-- separate, later concern, deliberately not fixed here.
--
-- Most payloads reuse @cardano-api@ types (addresses, policy/asset ids,
-- transaction references) so matching against decoded blocks needs no
-- conversion. The payment/delegation parts instead use 'CredentialHash' — a
-- bare 28-byte hash — so that matching is agnostic to whether a credential is a
-- key or a script.
module Cardano.Sieve.Selector
  ( Selector (..)
  , BootstrapFilter (..)
  , CredentialHash
  , credentialHashFromBytes
  , credentialHashToBytes
  )
where

import Cardano.Api
  ( AddressAny
  , AssetName
  , PolicyId
  , TxId
  , TxIn
  )

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word64)

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
