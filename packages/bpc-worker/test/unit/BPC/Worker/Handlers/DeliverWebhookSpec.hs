{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.DeliverWebhookSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T

import BPC.Worker.Handlers.DeliverWebhook

spec :: Spec
spec = do
  describe "DeliverWebhookPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "HMAC signature" $ do
    it "computes HMAC-SHA256" $ do
      let secret = "test-secret"
      let body = "{\"event\":\"test\"}"
      let sig = computeHmacSignature secret body
      T.isPrefixOf "sha256=" sig `shouldBe` True

    it "signature is verifiable" $ do
      pending

  describe "HTTP delivery" $ do
    it "successful delivery → DELIVERED" $ do
      pending

    it "HTTP 5xx → retry" $ do
      pending

    it "HTTP 4xx → no retry" $ do
      pending

    it "timeout → retry" $ do
      pending

  describe "Endpoint checks" $ do
    it "requires active endpoint" $ do
      pending

    it "rejects deactivated endpoint" $ do
      pending

  describe "Max attempts" $ do
    it "max attempts → DEAD_LETTER" $ do
      pending
