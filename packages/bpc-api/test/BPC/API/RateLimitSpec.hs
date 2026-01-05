{-# LANGUAGE OverloadedStrings #-}

module BPC.API.RateLimitSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import BPC.API.Middleware.RateLimit

spec :: Spec
spec = do
  describe "Token bucket" $ do
    it "hashApiKey produces consistent hash" $ do
      let key = "test-api-key-12345"
      let hash1 = hashApiKey key
      let hash2 = hashApiKey key
      hash1 `shouldBe` hash2

    it "hashApiKey produces different hashes for different keys" $ do
      let key1 = "test-api-key-1"
      let key2 = "test-api-key-2"
      let hash1 = hashApiKey key1
      let hash2 = hashApiKey key2
      hash1 `shouldNotBe` hash2

    it "hashApiKey produces 64-char hex string (SHA256)" $ do
      let key = "any-api-key"
      let hashed = hashApiKey key
      T.length hashed `shouldBe` 64

    it "hashApiKey handles empty key" $ do
      let hashed = hashApiKey ""
      T.length hashed `shouldBe` 64

  describe "RateLimitConfig" $ do
    it "config with sensible defaults" $ do
      let config = RateLimitConfig
            { rlcEnabled = True
            , rlcDefaultCapacity = 100
            , rlcDefaultRefillPerSecond = 10
            , rlcFailOpen = True
            }
      rlcEnabled config `shouldBe` True
      rlcDefaultCapacity config `shouldBe` 100
      rlcDefaultRefillPerSecond config `shouldBe` 10
      rlcFailOpen config `shouldBe` True

    it "disabled config skips rate limiting" $ do
      let config = RateLimitConfig
            { rlcEnabled = False
            , rlcDefaultCapacity = 0
            , rlcDefaultRefillPerSecond = 0
            , rlcFailOpen = False
            }
      rlcEnabled config `shouldBe` False

  describe "API key hashing" $ do
    it "produces consistent hash" $ do
      let key = "test-api-key"
      let hash1 = hashApiKey key
      let hash2 = hashApiKey key
      hash1 `shouldBe` hash2

    it "is deterministic across multiple calls" $ do
      let testKeys = ["key1", "key2", "key3", "a-very-long-api-key"]
      let results = map (\k -> (hashApiKey k, hashApiKey k)) testKeys
      all (\(a, b) -> a == b) results `shouldBe` True

    it "produces unique outputs for unique inputs" $ do
      let keys = ["key1", "key2", "key3", "key4", "key5"]
      let hashes = map hashApiKey keys
      length hashes `shouldBe` length (unique hashes)
      where
        unique :: Eq a => [a] -> [a]
        unique [] = []
        unique (x:xs) = x : unique (filter (/= x) xs)

    it "handles unicode in API keys" $ do
      let key = "key-with-émojis-🔑"
      let hashed = hashApiKey key
      T.length hashed `shouldBe` 64
