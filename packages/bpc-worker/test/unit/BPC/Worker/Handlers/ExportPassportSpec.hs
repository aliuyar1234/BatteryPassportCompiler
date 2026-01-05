{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.ExportPassportSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "ExportPassportPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "Precondition checks" $ do
    it "requires ACTIVE passport" $ do
      pending

    it "rejects non-ACTIVE passport" $ do
      pending

  describe "Export formats" $ do
    it "exports to JSON" $ do
      pending

    it "PDF export returns not implemented in MVP" $ do
      pending

  describe "Audit event" $ do
    it "emits PASSPORT_EXPORTED event" $ do
      pending
