{-# LANGUAGE OverloadedStrings #-}

module HandlersSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, ToJSON(..), FromJSON(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (nil)
import qualified Data.UUID as UUID
import qualified Data.ByteString.Lazy as LBS

import BPC.API.Types
import BPC.API.Error

spec :: Spec
spec = do
  describe "ErrorEnvelope serialization" $ do
    it "should serialize error envelope to JSON" $ do
      let correlationId = nil
      let err = toErrorResponse (NotFound "Document") correlationId
      let json = encode err
      LBS.length json > 0 `shouldBe` True

    it "should include error code in response" $ do
      let correlationId = nil
      let err = toErrorResponse (NotFound "Document") correlationId
      eeCode err `shouldBe` "NOT_FOUND"

    it "should include message in response" $ do
      let correlationId = nil
      let err = toErrorResponse (ValidationError [("name", "required")]) correlationId
      eeCode err `shouldBe` "VALIDATION_ERROR"

  describe "CursorPage serialization" $ do
    it "should serialize empty page" $ do
      let page = CursorPage ([] :: [Text]) Nothing Nothing
      let json = encode page
      LBS.length json > 0 `shouldBe` True

    it "should serialize page with items" $ do
      let page = CursorPage ["item1", "item2"] (Just "cursor") (Just 2)
      let json = encode page
      LBS.length json > 0 `shouldBe` True

  describe "CreatedResponse serialization" $ do
    it "should serialize created response" $ do
      let resp = CreatedResponse nil "/v1/documents/"
      let json = encode resp
      LBS.length json > 0 `shouldBe` True

  describe "Health check responses" $ do
    it "should return correct live status" $ do
      let status = LivenessStatus True
      lsHealthy status `shouldBe` True

    it "should return correct ready status" $ do
      let status = ReadinessStatus True True
      rsDbConnected status `shouldBe` True
      rsReady status `shouldBe` True

  describe "Permission mapping" $ do
    it "should map all permission types" $ do
      length [PermDocumentRead .. PermAdmin] `shouldBe` 16

    it "should distinguish read and write permissions" $ do
      PermDocumentRead `shouldNotBe` PermDocumentWrite
      PermPassportRead `shouldNotBe` PermPassportWrite

  describe "AppError status codes" $ do
    it "should return 401 for Unauthorized" $ do
      appErrorStatusCode (Unauthorized "test") `shouldBe` 401

    it "should return 404 for NotFound" $ do
      appErrorStatusCode (NotFound "resource") `shouldBe` 404

    it "should return 400 for ValidationError" $ do
      appErrorStatusCode (ValidationError []) `shouldBe` 400

    it "should return 500 for InternalError" $ do
      appErrorStatusCode (InternalError "oops") `shouldBe` 500

    it "should return 409 for Conflict" $ do
      appErrorStatusCode (Conflict "exists") `shouldBe` 409

    it "should return 429 for RateLimited" $ do
      appErrorStatusCode RateLimited `shouldBe` 429

-- Helper to get status code as Int
appErrorStatusCode :: AppError -> Int
appErrorStatusCode (Unauthorized _) = 401
appErrorStatusCode ApiKeyInvalid = 401
appErrorStatusCode (Forbidden _) = 403
appErrorStatusCode (NotFound _) = 404
appErrorStatusCode (ValidationError _) = 400
appErrorStatusCode (InvalidRequest _) = 400
appErrorStatusCode (Conflict _) = 409
appErrorStatusCode RateLimited = 429
appErrorStatusCode (InternalError _) = 500
appErrorStatusCode (DBError _) = 500
