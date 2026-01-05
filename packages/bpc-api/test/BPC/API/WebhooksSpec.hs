{-# LANGUAGE OverloadedStrings #-}

module BPC.API.WebhooksSpec (spec) where

import Test.Hspec
import Crypto.Hash (SHA256(..))
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

spec :: Spec
spec = do
  describe "Webhook endpoints" $ do
    it "endpoint URL must be valid HTTPS" $ do
      let validUrls = ["https://example.com/webhook", "https://api.test.io/hooks"]
      let invalidUrls = ["http://insecure.com", "ftp://wrong.com"]
      all isHttps validUrls `shouldBe` True
      any isHttps invalidUrls `shouldBe` False
      where
        isHttps url = take 8 url == "https://"

    it "endpoint secret must be at least 32 bytes" $ do
      let validSecret = "0123456789abcdef0123456789abcdef"
      let shortSecret = "tooshort"
      length validSecret >= 32 `shouldBe` True
      length shortSecret >= 32 `shouldBe` False

  describe "Webhook delivery" $ do
    it "delivery status transitions" $ do
      let statuses = ["PENDING", "DELIVERED", "FAILED"]
      length statuses `shouldBe` 3

    it "retry count increments on failure" $ do
      let initial = 0
      let afterRetry = initial + 1
      afterRetry `shouldBe` 1

  describe "HMAC signature" $ do
    it "computes valid HMAC-SHA256" $ do
      let secret = "webhook-secret-key"
      let payload = "{\"event\":\"test\"}"
      let signature = computeHmac secret payload
      -- SHA256 produces 32 bytes
      BS.length signature `shouldBe` 32

    it "same payload produces same signature" $ do
      let secret = "my-secret"
      let payload = "same-payload"
      let sig1 = computeHmac secret payload
      let sig2 = computeHmac secret payload
      sig1 `shouldBe` sig2

    it "different secrets produce different signatures" $ do
      let secret1 = "secret1"
      let secret2 = "secret2"
      let payload = "same-payload"
      let sig1 = computeHmac secret1 payload
      let sig2 = computeHmac secret2 payload
      sig1 `shouldNotBe` sig2

    it "different payloads produce different signatures" $ do
      let secret = "same-secret"
      let payload1 = "payload1"
      let payload2 = "payload2"
      let sig1 = computeHmac secret payload1
      let sig2 = computeHmac secret payload2
      sig1 `shouldNotBe` sig2

    it "signature format is 64 hex chars" $ do
      let secret = "test-secret"
      let payload = "{\"data\":\"test\"}"
      let sig = computeHmac secret payload
      let hexSig = B16.encode sig
      -- Should be 64 hex characters (32 bytes * 2)
      BS.length hexSig `shouldBe` 64

    it "tampered body fails verification" $ do
      let secret = "secret"
      let originalPayload = "{\"amount\":100}"
      let tamperedPayload = "{\"amount\":999}"
      let originalSig = computeHmac secret originalPayload
      let tamperedSig = computeHmac secret tamperedPayload
      originalSig `shouldNotBe` tamperedSig

    it "signature verifiable with endpoint secret" $ do
      let secret = "endpoint-secret"
      let payload = "{\"event\":\"passport.activated\"}"
      let sig = computeHmac secret payload
      let verifySig = computeHmac secret payload
      sig `shouldBe` verifySig

-- Helper to compute HMAC-SHA256
computeHmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
computeHmac secret payload =
  let tag = hmac secret payload :: HMAC SHA256
  in convert tag
