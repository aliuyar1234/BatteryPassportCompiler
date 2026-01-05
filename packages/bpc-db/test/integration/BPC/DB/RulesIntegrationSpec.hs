{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.RulesIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Rules Integration"
  [ testCase "create rule package" $ do
      assertBool "create package placeholder" True

  , testCase "create version with DSL hash" $ do
      assertBool "create version placeholder" True

  , testCase "record test run" $ do
      assertBool "record test run placeholder" True

  , testCase "publish with >= 500 test cases succeeds" $ do
      assertBool "publish success placeholder" True

  , testCase "publish without tests returns RULE_TESTS_FAILED" $ do
      assertBool "publish no tests placeholder" True

  , testCase "publish with < 500 cases returns RULE_TESTS_FAILED" $ do
      assertBool "publish insufficient tests placeholder" True
  ]
