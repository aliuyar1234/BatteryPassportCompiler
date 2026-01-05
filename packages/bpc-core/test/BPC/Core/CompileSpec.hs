{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.CompileSpec (tests) where

import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Data.UUID (nil)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.Compile
import BPC.Core.Rules.AST (Module (..))

tests :: TestTree
tests = testGroup "BPC.Core.Compile"
  [ testGroup "Size Limits"
      [ testCase "maxPayloadSize is 128KB" $ do
          maxPayloadSize @?= 131072

      , testCase "maxProofSize is 256KB" $ do
          maxProofSize @?= 262144

      , testCase "maxReceiptSize is 16KB" $ do
          maxReceiptSize @?= 16384
      ]

  , testGroup "Input Validation"
      [ testCase "reject non-SEALED snapshot" $ do
          let input = minimalInput { ciSnapshotStatus = "BUILDING" }
          case validateInput input of
            Left (SnapshotNotSealed _) -> pure ()
            _ -> fail "Expected SnapshotNotSealed error"

      , testCase "reject non-PUBLISHED rules" $ do
          let input = minimalInput { ciRulesStatus = "DRAFT" }
          case validateInput input of
            Left (RulesNotPublished _) -> pure ()
            _ -> fail "Expected RulesNotPublished error"

      , testCase "accept valid input" $ do
          assertBool "should accept" (isRight $ validateInput minimalInput)
      ]

  , testGroup "Size Checking"
      [ testCase "accept under limit" $ do
          assertBool "should accept" (isRight $ checkSize "payload" 1000 maxPayloadSize)

      , testCase "reject payload over limit" $ do
          case checkSize "payload" 200000 maxPayloadSize of
            Left (PayloadTooLarge actual limit) -> do
              actual @?= 200000
              limit @?= maxPayloadSize
            _ -> fail "Expected PayloadTooLarge error"

      , testCase "reject proof over limit" $ do
          case checkSize "proof" 300000 maxProofSize of
            Left (ProofTooLarge actual limit) -> do
              actual @?= 300000
              limit @?= maxProofSize
            _ -> fail "Expected ProofTooLarge error"

      , testCase "reject receipt over limit" $ do
          case checkSize "receipt" 20000 maxReceiptSize of
            Left (ReceiptTooLarge actual limit) -> do
              actual @?= 20000
              limit @?= maxReceiptSize
            _ -> fail "Expected ReceiptTooLarge error"
      ]

  , testGroup "Determinism"
      [ testProperty "prop_compile_deterministic" prop_compile_deterministic
      ]
  ]

-- Minimal valid input for testing
minimalInput :: CompileInput
minimalInput = CompileInput
  { ciSnapshotId = nil
  , ciSnapshotHash = "abc123"
  , ciSnapshotStatus = "SEALED"
  , ciRulesId = nil
  , ciRulesHash = "def456"
  , ciRulesStatus = "PUBLISHED"
  , ciRulesModule = Module []
  , ciFacts = Map.empty
  , ciTenantId = nil
  , ciPassportVersionId = nil
  }

-- Property: compilation is deterministic
prop_compile_deterministic :: Bool
prop_compile_deterministic =
  let result1 = compilePassportPure minimalInput
      result2 = compilePassportPure minimalInput
  in result1 == result2
