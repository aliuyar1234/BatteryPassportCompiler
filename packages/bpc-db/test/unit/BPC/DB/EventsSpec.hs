{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.EventsSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.DB.Repos.Events

tests :: TestTree
tests = testGroup "BPC.DB.Repos.Events"
  [ testGroup "BPC-EVENT-1 Hash Calculation"
      [ testCase "hash is deterministic" $ do
          let hash1 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload"
          let hash2 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload"
          hash1 @?= hash2

      , testCase "hash is 64 hex chars (SHA-256)" $ do
          let hash = calculateEventHash "document" UUID.nil "created" 1 Nothing "test"
          T.length hash @?= 64

      , testCase "different inputs produce different hashes" $ do
          let hash1 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload1"
          let hash2 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload2"
          assertBool "hashes should differ" (hash1 /= hash2)

      , testCase "prev_hash affects output" $ do
          let hash1 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload"
          let hash2 = calculateEventHash "document" UUID.nil "created" 1 (Just "abc") "payload"
          assertBool "hashes should differ" (hash1 /= hash2)

      , testCase "version affects output" $ do
          let hash1 = calculateEventHash "document" UUID.nil "created" 1 Nothing "payload"
          let hash2 = calculateEventHash "document" UUID.nil "created" 2 Nothing "payload"
          assertBool "hashes should differ" (hash1 /= hash2)
      ]

  , testGroup "Property Tests"
      [ testProperty "prop_hash_deterministic" $ \(n :: Int) ->
          let payload = BS.pack $ map fromIntegral [0..abs n `mod` 100]
              hash1 = calculateEventHash "test" UUID.nil "test" 1 Nothing payload
              hash2 = calculateEventHash "test" UUID.nil "test" 1 Nothing payload
          in hash1 == hash2

      , testProperty "prop_hash_length_64" $ \(n :: Int) ->
          let payload = BS.pack $ map fromIntegral [0..abs n `mod` 50]
              hash = calculateEventHash "test" UUID.nil "test" 1 Nothing payload
          in T.length hash == 64
      ]
  ]
