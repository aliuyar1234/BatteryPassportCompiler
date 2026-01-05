{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.DomainSpec (tests) where

import BPC.Core.Types.Domain
import Data.Aeson (decode, encode)
import Data.UUID (nil)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "BPC.Core.Types.Domain"
  [ testGroup "TenantId"
      [ testCase "Eq instance" $
          TenantId nil == TenantId nil @?= True

      , testCase "Ord instance" $
          compare (TenantId nil) (TenantId nil) @?= EQ

      , testCase "JSON round-trip" $
          decode (encode (TenantId nil)) @?= Just (TenantId nil)
      ]

  , testGroup "ActorId"
      [ testCase "JSON round-trip" $
          decode (encode (ActorId nil)) @?= Just (ActorId nil)
      ]

  , testGroup "PassportId"
      [ testCase "JSON round-trip" $
          decode (encode (PassportId nil)) @?= Just (PassportId nil)
      ]

  , testGroup "Type Safety"
      [ testCase "Different ID types are distinct" $
          -- This test just verifies the types exist and are distinct
          -- The real check is that the code compiles
          let tenantId = TenantId nil
              actorId = ActorId nil
           in (show tenantId /= show actorId) @?= True
      ]
  ]
