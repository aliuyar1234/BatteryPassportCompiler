{-# LANGUAGE OverloadedStrings #-}

module BPC.DB.PoolSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import BPC.DB.Pool

tests :: TestTree
tests = testGroup "BPC.DB.Pool"
  [ testGroup "DBConfig"
      [ testCase "defaultDBConfig has expected values" $ do
          dbHost defaultDBConfig @?= "localhost"
          dbPort defaultDBConfig @?= 5432
          dbName defaultDBConfig @?= "bpc"
          dbPoolSize defaultDBConfig @?= 10

      , testCase "buildConnString includes all fields" $ do
          let cfg = defaultDBConfig
                { dbHost = "db.example.com"
                , dbPort = 5433
                , dbName = "testdb"
                , dbUser = "testuser"
                , dbPassword = "secret"
                }
          let connStr = buildConnString cfg
          assertBool "includes host" ("host=db.example.com" `elem` words (show connStr))
      ]

  , testGroup "Pool Configuration"
      [ testCase "pool size is configurable" $ do
          let cfg = defaultDBConfig { dbPoolSize = 20 }
          dbPoolSize cfg @?= 20

      , testCase "timeout is configurable" $ do
          let cfg = defaultDBConfig { dbPoolTimeout = 60 }
          dbPoolTimeout cfg @?= 60
      ]
  ]
