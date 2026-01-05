{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.HashSpec (tests) where

import BPC.Core.Hash
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "BPC.Core.Hash"
  [ testGroup "sha256Hex"
      [ testCase "matches SSOT fixture" $
          -- From SSOT 7.2: sha256("{\"a\":1,\"b\":2}") = 43258cff...
          sha256Hex "{\"a\":1,\"b\":2}"
            @?= "43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777"

      , testProperty "deterministic" $ \bs ->
          sha256Hex bs == sha256Hex bs

      , testProperty "different inputs give different hashes" $ \(bs1, bs2) ->
          bs1 /= bs2 ==> sha256Hex bs1 /= sha256Hex bs2
      ]

  , testGroup "sha256"
      [ testCase "returns 32 bytes" $
          BS.length (sha256 "test") @?= 32

      , testProperty "deterministic raw hash" $ \bs ->
          sha256 bs == sha256 bs
      ]

  , testGroup "base32NoPad"
      [ testCase "contains only valid characters" $
          let encoded = base32NoPad "test"
           in T.all (\c -> c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" :: String)) encoded
                @?= True

      , testCase "no padding characters" $
          T.all (/= '=') (base32NoPad "test") @?= True

      , testProperty "deterministic encoding" $ \bs ->
          base32NoPad bs == base32NoPad bs

      , testProperty "only valid base32 characters" $ \bs ->
          let encoded = base32NoPad bs
           in T.all (`elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" :: String)) encoded
      ]
  ]
