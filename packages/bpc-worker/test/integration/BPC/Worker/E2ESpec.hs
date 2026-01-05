{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.E2ESpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "End-to-end pipeline" $ do
    it "upload → parse → seal → compile → sign → qr → active" $ do
      pending

    it "auto-chained jobs (compile → sign → qr)" $ do
      pending

  describe "Retry behavior" $ do
    it "verifies exponential backoff timing" $ do
      pending

  describe "Dead letter" $ do
    it "10 failures → DEAD_LETTER" $ do
      pending
