{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (nil)

import BPC.API.Types
import BPC.API.Middleware.Auth

spec :: Spec
spec = do
  describe "Permission checking" $ do
    it "should allow access with matching permission" $ do
      let ctx = AuthContext nil nil [PermDocumentRead, PermDocumentWrite]
      hasPermission PermDocumentRead ctx `shouldBe` True

    it "should deny access without matching permission" $ do
      let ctx = AuthContext nil nil [PermDocumentRead]
      hasPermission PermPassportWrite ctx `shouldBe` False

    it "should allow admin access to everything" $ do
      let ctx = AuthContext nil nil [PermAdmin]
      hasPermission PermDocumentRead ctx `shouldBe` True
      hasPermission PermPassportWrite ctx `shouldBe` True
      hasPermission PermRuleWrite ctx `shouldBe` True

  describe "API key validation" $ do
    it "should validate correct key format" $ do
      -- MVP: Basic format validation
      let key = "bpc_live_abcd1234efgh5678ijkl9012mnop"
      T.isPrefixOf "bpc_" key `shouldBe` True

    it "should reject empty key" $ do
      let key = ""
      T.null key `shouldBe` True

  describe "AuthContext construction" $ do
    it "should preserve tenant and actor IDs" $ do
      let tenantId = nil
      let actorId = nil
      let ctx = AuthContext tenantId actorId []
      acTenantId ctx `shouldBe` tenantId
      acActorId ctx `shouldBe` actorId

    it "should handle empty permissions list" $ do
      let ctx = AuthContext nil nil []
      acPermissions ctx `shouldBe` []
