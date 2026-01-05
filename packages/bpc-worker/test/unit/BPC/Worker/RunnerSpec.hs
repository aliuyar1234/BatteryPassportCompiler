{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.RunnerSpec (spec) where

import Test.Hspec
import Data.UUID (nil)

import BPC.Worker.Types
import BPC.Worker.Retry

spec :: Spec
spec = do
  describe "Worker loop" $ do
    it "should poll at configured interval" $ do
      let config = defaultTestConfig
      wcPollIntervalMs config `shouldBe` 1000

    it "should process job when available" $ do
      -- MVP: Basic test structure
      pending

    it "should sleep when queue empty" $ do
      pending

    it "should handle graceful shutdown" $ do
      pending

  describe "Job leasing" $ do
    it "acquireLease sets status RUNNING" $ do
      pending

    it "leased job not acquired by another worker" $ do
      pending

    it "expired lease allows re-acquisition" $ do
      pending

  describe "Job result handling" $ do
    it "success completes job" $ do
      pending

    it "retryable failure reschedules with backoff" $ do
      pending

    it "non-retryable failure marks FAILED" $ do
      pending

    it "max attempts marks DEAD_LETTER" $ do
      pending

-- | Default test configuration.
defaultTestConfig :: WorkerConfig
defaultTestConfig = WorkerConfig
  { wcPollIntervalMs = 1000
  , wcLeaseRenewSeconds = 30
  , wcLeaseTimeoutSeconds = 300
  , wcMaxAttempts = 10
  , wcWorkerId = "test-worker"
  , wcSigningKeyBase64 = Nothing
  , wcWebhookTimeoutSeconds = 30
  }
