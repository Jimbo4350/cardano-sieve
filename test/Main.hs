-- | Test suite for @cardano-sieve@.
--
-- Phase A stands the harness up with a trivial check on the 'Selector' ADT.
-- Richer properties (e.g. @rollforward . rollback = id@, once rollback across
-- the schema exists) land in later phases.
module Main (main) where

import Cardano.Sieve.Selector (BootstrapFilter (..), Selector (SelectAll))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Cardano.Sieve.Selector"
    [ testCase "SelectAll equality distinguishes the bootstrap filter" $ do
        (SelectAll IncludeBootstrap == SelectAll IncludeBootstrap) @?= True
        (SelectAll IncludeBootstrap == SelectAll OnlyShelley) @?= False
    ]
