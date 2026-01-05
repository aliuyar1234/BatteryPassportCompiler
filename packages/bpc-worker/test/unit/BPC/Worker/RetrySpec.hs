{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.RetrySpec (spec) where

import Test.Hspec
import Data.Time (NominalDiffTime)

import BPC.Worker.Retry
import BPC.Worker.Types

spec :: Spec
spec = do
  describe "computeBackoff" $ do
    it "computeBackoff(1) = 2s" $ do
      computeBackoff 1 `shouldBe` (2 :: NominalDiffTime)

    it "computeBackoff(2) = 4s" $ do
      computeBackoff 2 `shouldBe` (4 :: NominalDiffTime)

    it "computeBackoff(5) = 32s" $ do
      computeBackoff 5 `shouldBe` (32 :: NominalDiffTime)

    it "computeBackoff(10) = 1024s (capped)" $ do
      computeBackoff 10 `shouldBe` (1024 :: NominalDiffTime)

    it "computeBackoff(15) = 1024s (capped at max)" $ do
      computeBackoff 15 `shouldBe` (1024 :: NominalDiffTime)

    it "computeBackoff(0) = 1s" $ do
      computeBackoff 0 `shouldBe` (1 :: NominalDiffTime)

  describe "maxAttempts" $ do
    it "should be 10 per SSOT" $ do
      maxAttempts `shouldBe` 10

  describe "isRetryableError" $ do
    it "HERetryable is retryable" $ do
      isRetryableError (HERetryable "test") `shouldBe` True

    it "HENonRetryable is not retryable" $ do
      isRetryableError (HENonRetryable "test") `shouldBe` False

    it "HEValidation is not retryable" $ do
      isRetryableError (HEValidation "test") `shouldBe` False

    it "HENotFound is not retryable" $ do
      isRetryableError (HENotFound "test") `shouldBe` False

    it "HEPrecondition is not retryable" $ do
      isRetryableError (HEPrecondition "test") `shouldBe` False

    it "HEInternal is retryable (may be transient)" $ do
      isRetryableError (HEInternal "test") `shouldBe` True

  describe "shouldRetry" $ do
    it "should retry retryable errors under max attempts" $ do
      shouldRetry 3 (HERetryable "test") `shouldBe` True

    it "should not retry at max attempts" $ do
      shouldRetry maxAttempts (HERetryable "test") `shouldBe` False

    it "should not retry non-retryable errors" $ do
      shouldRetry 1 (HENonRetryable "test") `shouldBe` False
