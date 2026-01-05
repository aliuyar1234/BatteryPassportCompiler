{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.ProofSpec (tests) where

import Data.Either (isRight)
import qualified Data.Aeson as Aeson
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.Proof

tests :: TestTree
tests = testGroup "BPC.Core.Proof"
  [ testGroup "Proof Building"
      [ testCase "empty nodes produces valid proof" $ do
          let proof = buildProof []
          prVersion proof @?= "BPC-PROOF-1"
          prNodes proof @?= []

      , testCase "single node proof" $ do
          let node = ProofNode
                { pnId = 0
                , pnType = CONST
                , pnValue = Aeson.Number 42
                , pnInputIds = []
                , pnFieldPath = Nothing
                , pnHash = ""
                }
          let proof = buildProof [node]
          length (prNodes proof) @?= 1
          prVersion proof @?= "BPC-PROOF-1"

      , testCase "proof version is BPC-PROOF-1" $ do
          let proof = buildProof []
          prVersion proof @?= "BPC-PROOF-1"
      ]

  , testGroup "Node Types"
      [ testCase "CONST type" $ do
          let node = ProofNode 0 CONST Aeson.Null [] Nothing ""
          pnType node @?= CONST

      , testCase "FACT_GET type" $ do
          let node = ProofNode 0 FACT_GET Aeson.Null [] Nothing ""
          pnType node @?= FACT_GET

      , testCase "FIELD_REF type" $ do
          let node = ProofNode 0 FIELD_REF Aeson.Null [] Nothing ""
          pnType node @?= FIELD_REF

      , testCase "OP type" $ do
          let node = ProofNode 0 OP Aeson.Null [] Nothing ""
          pnType node @?= OP

      , testCase "ASSERT type" $ do
          let node = ProofNode 0 ASSERT Aeson.Null [] Nothing ""
          pnType node @?= ASSERT

      , testCase "COMPLIANCE_EMIT type" $ do
          let node = ProofNode 0 COMPLIANCE_EMIT Aeson.Null [] Nothing ""
          pnType node @?= COMPLIANCE_EMIT
      ]

  , testGroup "Node Hashing"
      [ testCase "node hash is deterministic" $ do
          let hash1 = computeNodeHash CONST (Aeson.Number 42) []
          let hash2 = computeNodeHash CONST (Aeson.Number 42) []
          hash1 @?= hash2

      , testCase "different values produce different hashes" $ do
          let hash1 = computeNodeHash CONST (Aeson.Number 42) []
          let hash2 = computeNodeHash CONST (Aeson.Number 43) []
          assertBool "should differ" (hash1 /= hash2)

      , testCase "different types produce different hashes" $ do
          let hash1 = computeNodeHash CONST (Aeson.Number 42) []
          let hash2 = computeNodeHash OP (Aeson.Number 42) []
          assertBool "should differ" (hash1 /= hash2)
      ]

  , testGroup "Proof Verification"
      [ testCase "empty proof verifies" $ do
          let proof = buildProof []
          assertBool "should verify" (isRight $ verifyProof proof)

      , testCase "single node proof verifies" $ do
          let node = ProofNode
                { pnId = 0
                , pnType = CONST
                , pnValue = Aeson.Number 42
                , pnInputIds = []
                , pnFieldPath = Nothing
                , pnHash = ""
                }
          let proof = buildProof [node]
          assertBool "should verify" (isRight $ verifyProof proof)
      ]

  , testGroup "Field Index"
      [ testCase "build field index from nodes" $ do
          let node1 = ProofNode 0 CONST Aeson.Null [] (Just "a.b") ""
          let node2 = ProofNode 1 CONST Aeson.Null [] (Just "c.d") ""
          let idx = buildFieldIndex [node1, node2]
          length idx @?= 2
      ]

  , testGroup "Property Tests"
      [ testProperty "prop_proof_hash_deterministic" $ \n ->
          let hash1 = computeNodeHash CONST (Aeson.Number $ fromIntegral (n :: Int)) []
              hash2 = computeNodeHash CONST (Aeson.Number $ fromIntegral n) []
          in hash1 == hash2
      ]
  ]
