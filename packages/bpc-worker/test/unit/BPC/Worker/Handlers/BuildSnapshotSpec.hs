{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.BuildSnapshotSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "BuildSnapshotPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "Snapshot status transition" $ do
    it "BUILDING → READY" $ do
      pending

    it "empty snapshot → still READY" $ do
      pending

    it "already READY → error" $ do
      pending

    it "already SEALED → error" $ do
      pending

  describe "Audit event" $ do
    it "emits SNAPSHOT_READY event" $ do
      pending
