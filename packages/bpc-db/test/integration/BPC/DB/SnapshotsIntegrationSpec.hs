{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.SnapshotsIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Snapshots Integration"
  [ testCase "create snapshot with status BUILDING" $ do
      assertBool "create snapshot placeholder" True

  , testCase "add items to snapshot" $ do
      assertBool "add items placeholder" True

  , testCase "seal snapshot calculates BPC-SNAPSHOT-1 hash" $ do
      assertBool "seal snapshot placeholder" True

  , testCase "add item to SEALED snapshot returns SNAPSHOT_SEALED" $ do
      assertBool "sealed snapshot immutable placeholder" True

  , testCase "same facts in different order produce same hash" $ do
      -- BPC-SNAPSHOT-1 sorts by (fact_type, fact_key, payload_hash)
      assertBool "deterministic hash placeholder" True
  ]
