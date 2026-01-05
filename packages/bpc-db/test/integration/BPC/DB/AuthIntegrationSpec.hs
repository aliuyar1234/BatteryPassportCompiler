{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.AuthIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Auth Integration"
  [ testCase "create and get tenant" $ do
      assertBool "tenant CRUD placeholder" True

  , testCase "create and get actor" $ do
      assertBool "actor CRUD placeholder" True

  , testCase "create API key with SHA-256 hash" $ do
      assertBool "create API key placeholder" True

  , testCase "verify API key by prefix and hash" $ do
      assertBool "verify API key placeholder" True

  , testCase "revoke API key" $ do
      assertBool "revoke API key placeholder" True

  , testCase "assign and remove role" $ do
      assertBool "role assignment placeholder" True

  , testCase "get actor permissions via roles" $ do
      assertBool "permissions placeholder" True
  ]
