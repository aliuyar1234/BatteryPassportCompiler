{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.ConcurrencySpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Concurrent workers" $ do
    it "10 workers, no duplicate processing" $ do
      pending

    it "100 jobs processed exactly once" $ do
      pending

  describe "Race conditions" $ do
    it "FOR UPDATE SKIP LOCKED prevents races" $ do
      pending

    it "lease renewal prevents timeout takeover" $ do
      pending
