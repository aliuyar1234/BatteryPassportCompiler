{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.ReceiptSpec (tests) where

import qualified Data.UUID as UUID
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.Receipt

tests :: TestTree
tests = testGroup "BPC.Core.Receipt"
  [ testGroup "Receipt Building"
      [ testCase "build unsigned receipt from input" $ do
          let input = minimalReceiptInput
          let receipt = buildReceiptUnsigned input
          ruVersion receipt @?= "BPC-RECEIPT-1"

      , testCase "receipt version is BPC-RECEIPT-1" $ do
          let receipt = buildReceiptUnsigned minimalReceiptInput
          ruVersion receipt @?= "BPC-RECEIPT-1"

      , testCase "receipt contains all required fields" $ do
          let receipt = buildReceiptUnsigned minimalReceiptInput
          ruPassportVersionId receipt @?= riPassportVersionId minimalReceiptInput
          ruTenantId receipt @?= riTenantId minimalReceiptInput
          ruSnapshotId receipt @?= riSnapshotId minimalReceiptInput
          ruSnapshotHash receipt @?= riSnapshotHash minimalReceiptInput
          ruRulesId receipt @?= riRulesId minimalReceiptInput
          ruRulesHash receipt @?= riRulesHash minimalReceiptInput
          ruPayloadHash receipt @?= riPayloadHash minimalReceiptInput
          ruProofHash receipt @?= riProofHash minimalReceiptInput
      ]

  , testGroup "Receipt Hashing"
      [ testCase "hash is deterministic" $ do
          let receipt = buildReceiptUnsigned minimalReceiptInput
          let hash1 = hashReceiptUnsigned receipt
          let hash2 = hashReceiptUnsigned receipt
          hash1 @?= hash2

      , testCase "different receipts have different hashes" $ do
          let receipt1 = buildReceiptUnsigned minimalReceiptInput
          let receipt2 = buildReceiptUnsigned $ minimalReceiptInput
                { riPayloadHash = "different_hash" }
          let hash1 = hashReceiptUnsigned receipt1
          let hash2 = hashReceiptUnsigned receipt2
          assertBool "should differ" (hash1 /= hash2)
      ]

  , testGroup "Property Tests"
      [ testProperty "prop_receipt_hash_deterministic" $ \n ->
          let input = minimalReceiptInput { riPayloadHash = show (n :: Int) }
              receipt = buildReceiptUnsigned input
              hash1 = hashReceiptUnsigned receipt
              hash2 = hashReceiptUnsigned receipt
          in hash1 == hash2
      ]
  ]

-- Minimal receipt input for testing
minimalReceiptInput :: ReceiptInput
minimalReceiptInput = ReceiptInput
  { riPassportVersionId = UUID.nil
  , riTenantId = UUID.nil
  , riSnapshotId = UUID.nil
  , riSnapshotHash = "snapshot_hash_abc123"
  , riRulesId = UUID.nil
  , riRulesHash = "rules_hash_def456"
  , riPayloadHash = "payload_hash_ghi789"
  , riProofHash = "proof_hash_jkl012"
  }
