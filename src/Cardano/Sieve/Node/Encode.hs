{-# LANGUAGE ImportQualifiedPost #-}

-- | Serialise matched outputs to the bytes the schema stores: the encode
-- direction between 'DecodedOutput' and 'applyBlock'.
module Cardano.Sieve.Node.Encode
  ( selectedStored
  , toStored
  )
where

import Cardano.Api (AssetId (AssetId), BlockInMode, Value, serialiseToRawBytes)

import Cardano.Sieve.Node.Decode (DecodedOutput (..), encodeOutputRef, selectedOutputs)
import Cardano.Sieve.Node.Insert (StoredOutput (..))
import Cardano.Sieve.Selector (Selector, delegationHash, paymentHash)
import Cardano.Sieve.Value (encodeValue)

import Data.ByteString (ByteString)
import Data.Set qualified as Set
import GHC.Exts (toList)

-- | The selected outputs of a block, as rows for 'applyBlock'.
selectedStored :: [Selector] -> BlockInMode -> [StoredOutput]
selectedStored selectors = map toStored . selectedOutputs selectors

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

-- | The distinct (policy id, asset name) pairs of the positive-quantity assets
-- in a value (ada excluded). Both are raw bytes; the asset name may be empty.
assetsOf :: Value -> [(ByteString, ByteString)]
assetsOf v =
  Set.toList
    ( Set.fromList
        [(serialiseToRawBytes pid, serialiseToRawBytes name) | (AssetId pid name, q) <- toList v, q > 0]
    )
