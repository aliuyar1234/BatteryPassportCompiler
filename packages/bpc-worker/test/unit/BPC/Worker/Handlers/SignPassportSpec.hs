{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.SignPassportSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "SignPassportPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "Signing key" $ do
    it "requires configured signing key" $ do
      pending

    it "decodes base64 ED25519 key" $ do
      pending

  describe "Signature" $ do
    it "creates ED25519 signature" $ do
      pending

    it "signature verifiable with public key" $ do
      pending

  describe "Status transition" $ do
    it "COMPILING → SIGNED" $ do
      pending

  describe "Job chaining" $ do
    it "enqueues GENERATE_QR job" $ do
      pending

  describe "Audit event" $ do
    it "emits PASSPORT_SIGNED event" $ do
      pending
