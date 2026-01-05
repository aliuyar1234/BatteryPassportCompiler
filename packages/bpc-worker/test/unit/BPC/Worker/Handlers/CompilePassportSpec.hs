{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.CompilePassportSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "CompilePassportPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "Precondition checks" $ do
    it "requires SEALED snapshot" $ do
      pending

    it "requires PUBLISHED rules" $ do
      pending

  describe "Compilation" $ do
    it "valid inputs → PassportVersion created" $ do
      pending

    it "compilation is deterministic (golden test)" $ do
      pending

  describe "Size limits" $ do
    it "rejects payload > 128KB" $ do
      pending

    it "rejects proof > 256KB" $ do
      pending

    it "rejects receipt > 16KB" $ do
      pending

  describe "Job chaining" $ do
    it "enqueues SIGN_PASSPORT job" $ do
      pending

  describe "Audit event" $ do
    it "emits PASSPORT_COMPILED event" $ do
      pending
