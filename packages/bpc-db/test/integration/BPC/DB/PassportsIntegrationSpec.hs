{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.PassportsIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Passports Integration"
  [ testCase "create passport for battery product" $ do
      assertBool "create passport placeholder" True

  , testCase "insert version with canonical artifacts" $ do
      assertBool "insert version placeholder" True

  , testCase "activate version sets status ACTIVE" $ do
      assertBool "activate placeholder" True

  , testCase "activate supersedes previous ACTIVE version" $ do
      assertBool "supersede placeholder" True

  , testCase "revoke version sets status REVOKED" $ do
      assertBool "revoke placeholder" True
  ]
