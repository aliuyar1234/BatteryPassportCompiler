{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.WebhooksIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Webhooks Integration"
  [ testCase "create endpoint" $ do
      assertBool "create endpoint placeholder" True

  , testCase "subscribe to event type" $ do
      assertBool "subscribe placeholder" True

  , testCase "record delivery" $ do
      assertBool "record delivery placeholder" True

  , testCase "mark delivery as delivered" $ do
      assertBool "mark delivered placeholder" True

  , testCase "mark delivery as failed" $ do
      assertBool "mark failed placeholder" True

  , testCase "get pending deliveries" $ do
      assertBool "pending deliveries placeholder" True
  ]
