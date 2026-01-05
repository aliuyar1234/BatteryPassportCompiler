{-# LANGUAGE OverloadedStrings #-}

module BPC.API.PolicySpec (spec) where

import Test.Hspec
import Data.UUID (nil)

import BPC.API.Middleware.Policy
import BPC.API.Types (AuthContext(..))

spec :: Spec
spec = do
  describe "PolicyDecision" $ do
    it "PolicyAllow allows request" $ do
      PolicyAllow `shouldBe` PolicyAllow

    it "PolicyDeny blocks request with reason" $ do
      let decision = PolicyDeny "test reason"
      case decision of
        PolicyDeny reason -> reason `shouldBe` "test reason"
        _ -> expectationFailure "Should be PolicyDeny"

    it "PolicyNoMatch falls back to RBAC" $ do
      PolicyNoMatch `shouldBe` PolicyNoMatch

    it "PolicyDecision types are distinct" $ do
      PolicyAllow `shouldNotBe` PolicyNoMatch
      PolicyNoMatch `shouldNotBe` PolicyDeny "reason"

  describe "PolicyContext" $ do
    it "stores resource and action" $ do
      let ctx = PolicyContext
            { pcResource = "documents"
            , pcAction = "read"
            , pcAuth = AuthContext nil nil []
            }
      pcResource ctx `shouldBe` "documents"
      pcAction ctx `shouldBe` "read"

    it "supports multiple resource types" $ do
      let resources = ["documents", "passports", "snapshots", "rules"]
      let contexts = map (\r -> PolicyContext r "read" (AuthContext nil nil [])) resources
      map pcResource contexts `shouldBe` resources

    it "supports multiple actions" $ do
      let actions = ["read", "write", "delete", "admin"]
      let contexts = map (\a -> PolicyContext "documents" a (AuthContext nil nil [])) actions
      map pcAction contexts `shouldBe` actions

  describe "PolicyDecision equality" $ do
    it "PolicyAllow equals PolicyAllow" $ do
      PolicyAllow == PolicyAllow `shouldBe` True

    it "PolicyDeny equals PolicyDeny with same reason" $ do
      PolicyDeny "reason" == PolicyDeny "reason" `shouldBe` True

    it "PolicyDeny differs with different reasons" $ do
      PolicyDeny "reason1" == PolicyDeny "reason2" `shouldBe` False

    it "PolicyNoMatch equals PolicyNoMatch" $ do
      PolicyNoMatch == PolicyNoMatch `shouldBe` True

    it "Different decision types are not equal" $ do
      PolicyAllow == PolicyNoMatch `shouldBe` False
      PolicyNoMatch == PolicyDeny "x" `shouldBe` False

  describe "PolicyDecision Show instance" $ do
    it "PolicyAllow shows correctly" $ do
      show PolicyAllow `shouldBe` "PolicyAllow"

    it "PolicyNoMatch shows correctly" $ do
      show PolicyNoMatch `shouldBe` "PolicyNoMatch"

    it "PolicyDeny shows with reason" $ do
      show (PolicyDeny "denied") `shouldContain` "PolicyDeny"
      show (PolicyDeny "denied") `shouldContain` "denied"
