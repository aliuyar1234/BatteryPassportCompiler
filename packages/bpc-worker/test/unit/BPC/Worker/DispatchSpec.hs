{-# LANGUAGE OverloadedStrings #-}

module BPC.Worker.DispatchSpec (spec) where

import Test.Hspec
import Data.Text (Text)

import BPC.Worker.Types

spec :: Spec
spec = do
  describe "Job type parsing" $ do
    it "parses INGEST_DOCUMENT" $ do
      parseJobTypeText "INGEST_DOCUMENT" `shouldBe` Just JTIngestDocument

    it "parses PARSE_FACTS" $ do
      parseJobTypeText "PARSE_FACTS" `shouldBe` Just JTParseFacts

    it "parses BUILD_SNAPSHOT" $ do
      parseJobTypeText "BUILD_SNAPSHOT" `shouldBe` Just JTBuildSnapshot

    it "parses COMPILE_PASSPORT" $ do
      parseJobTypeText "COMPILE_PASSPORT" `shouldBe` Just JTCompilePassport

    it "parses SIGN_PASSPORT" $ do
      parseJobTypeText "SIGN_PASSPORT" `shouldBe` Just JTSignPassport

    it "parses GENERATE_QR" $ do
      parseJobTypeText "GENERATE_QR" `shouldBe` Just JTGenerateQR

    it "parses EXPORT_PASSPORT" $ do
      parseJobTypeText "EXPORT_PASSPORT" `shouldBe` Just JTExportPassport

    it "parses RUN_RULE_TESTS" $ do
      parseJobTypeText "RUN_RULE_TESTS" `shouldBe` Just JTRunRuleTests

    it "parses DELIVER_WEBHOOK" $ do
      parseJobTypeText "DELIVER_WEBHOOK" `shouldBe` Just JTDeliverWebhook

    it "returns Nothing for unknown type" $ do
      parseJobTypeText "UNKNOWN_JOB" `shouldBe` Nothing

  describe "Job dispatching" $ do
    it "dispatches to correct handler" $ do
      pending

    it "throws UnsupportedJobType for unknown types" $ do
      pending

-- | Parse job type from text.
parseJobTypeText :: Text -> Maybe JobType
parseJobTypeText t = case t of
  "INGEST_DOCUMENT" -> Just JTIngestDocument
  "PARSE_FACTS" -> Just JTParseFacts
  "BUILD_SNAPSHOT" -> Just JTBuildSnapshot
  "COMPILE_PASSPORT" -> Just JTCompilePassport
  "SIGN_PASSPORT" -> Just JTSignPassport
  "GENERATE_QR" -> Just JTGenerateQR
  "EXPORT_PASSPORT" -> Just JTExportPassport
  "RUN_RULE_TESTS" -> Just JTRunRuleTests
  "DELIVER_WEBHOOK" -> Just JTDeliverWebhook
  _ -> Nothing
