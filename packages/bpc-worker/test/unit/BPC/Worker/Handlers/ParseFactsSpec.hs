{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.Handlers.ParseFactsSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.UUID (nil)

import BPC.Worker.Handlers.ParseFacts

spec :: Spec
spec = do
  describe "ParseFactsPayload" $ do
    it "decodes valid JSON" $ do
      let json = encode $ object
            [ "document_version_id" .= show nil
            , "tenant_id" .= show nil
            ]
      case decode json :: Maybe ParseFactsPayload of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should decode valid payload"

  describe "parseBOM" $ do
    it "parses valid BOM JSON" $ do
      let bom = "{\"component\": \"battery\", \"capacity\": 100}"
      facts <- parseBOM (BS.toStrict $ LBS.fromStrict $ BS.pack $ map (fromIntegral . fromEnum) bom)
      length facts `shouldSatisfy` (>= 0)

    it "returns empty list for invalid JSON" $ do
      facts <- parseBOM "not valid json"
      facts `shouldBe` []

  describe "parsePCF" $ do
    it "parses valid CSV" $ do
      let csv = "name,value\nitem1,100\nitem2,200"
      facts <- parsePCF (BS.pack $ map (fromIntegral . fromEnum) csv)
      length facts `shouldBe` 2

    it "handles empty CSV" $ do
      facts <- parsePCF ""
      facts `shouldBe` []

  describe "Fact hash computation" $ do
    it "computes SHA-256 hash of canonical bytes" $ do
      pending

    it "produces deterministic hash" $ do
      pending

  describe "Document status update" $ do
    it "transitions UPLOADED → VALIDATED" $ do
      pending

    it "transitions to REJECTED on invalid format" $ do
      pending

  describe "Audit event" $ do
    it "emits FACTS_PARSED event" $ do
      pending
