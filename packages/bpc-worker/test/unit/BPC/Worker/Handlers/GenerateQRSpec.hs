{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.GenerateQRSpec (spec) where

import Test.Hspec
import Data.UUID (nil)
import Data.List (isPrefixOf)

import BPC.Worker.Handlers.GenerateQR

spec :: Spec
spec = do
  describe "GenerateQRPayload" $ do
    it "decodes valid JSON" $ do
      pending

  describe "QR payload format" $ do
    it "matches BPC-QR-1 format" $ do
      let payload = buildQrPayloadString nil "hash1" "hash2" "hash3"
      -- Format: BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
      payload `shouldSatisfy` \p -> "BPC1|" `isPrefixOf` p

    it "uses base32 without padding for hashes" $ do
      pending

  describe "QR generation" $ do
    it "generates valid PNG" $ do
      pending

    it "QR code is scannable" $ do
      pending

  describe "Status" $ do
    it "status remains SIGNED after QR generation" $ do
      pending

  describe "Audit event" $ do
    it "emits QR_GENERATED event" $ do
      pending
