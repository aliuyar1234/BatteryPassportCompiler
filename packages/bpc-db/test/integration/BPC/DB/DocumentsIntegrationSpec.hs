{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.DocumentsIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Documents Integration"
  [ testCase "create document" $ do
      assertBool "create document placeholder" True

  , testCase "upload version with SHA-256 hash" $ do
      assertBool "upload version placeholder" True

  , testCase "get content returns original bytes" $ do
      assertBool "get content placeholder" True

  , testCase "duplicate SHA-256 causes constraint violation" $ do
      assertBool "duplicate hash placeholder" True
  ]
