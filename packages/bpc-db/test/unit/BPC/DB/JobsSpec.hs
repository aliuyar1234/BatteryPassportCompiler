{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.JobsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.DB.Repos.Jobs

tests :: TestTree
tests = testGroup "BPC.DB.Repos.Jobs"
  [ testGroup "Backoff Calculation"
      [ testCase "backoff for 0 attempts is 1 second" $
          calculateBackoff 0 @?= 1

      , testCase "backoff for 1 attempt is 2 seconds" $
          calculateBackoff 1 @?= 2

      , testCase "backoff for 2 attempts is 4 seconds" $
          calculateBackoff 2 @?= 4

      , testCase "backoff for 3 attempts is 8 seconds" $
          calculateBackoff 3 @?= 8

      , testCase "backoff for 5 attempts is 32 seconds" $
          calculateBackoff 5 @?= 32

      , testCase "backoff for 10 attempts is capped at 1024 seconds" $
          calculateBackoff 10 @?= 1024

      , testCase "backoff for 20 attempts is still capped at 1024" $
          calculateBackoff 20 @?= 1024
      ]

  , testGroup "Property Tests"
      [ testProperty "prop_backoff_exponential" $ \n ->
          let attempts = abs n `mod` 10
              expected = min (2 ^ attempts) 1024
          in realToFrac (calculateBackoff attempts) == (fromIntegral expected :: Double)

      , testProperty "prop_backoff_capped" $ \n ->
          let attempts = abs n
          in calculateBackoff attempts <= 1024
      ]
  ]
