{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.Rules.BuiltinsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import BPC.Core.Rules.Builtins

tests :: TestTree
tests = testGroup "BPC.Core.Rules.Builtins"
  [ testGroup "Builtin Lookup"
      [ testCase "lookup getFact" $ do
          lookupBuiltin "getFact" @?= Just BGetFact

      , testCase "lookup getFactsByPrefix" $ do
          lookupBuiltin "getFactsByPrefix" @?= Just BGetFactsByPrefix

      , testCase "lookup recordGet" $ do
          lookupBuiltin "recordGet" @?= Just BRecordGet

      , testCase "lookup isSome" $ do
          lookupBuiltin "isSome" @?= Just BIsSome

      , testCase "lookup unwrapOr" $ do
          lookupBuiltin "unwrapOr" @?= Just BUnwrapOr

      , testCase "lookup requireSome" $ do
          lookupBuiltin "requireSome" @?= Just BRequireSome

      , testCase "lookup toDec" $ do
          lookupBuiltin "toDec" @?= Just BToDec

      , testCase "lookup toQty" $ do
          lookupBuiltin "toQty" @?= Just BToQty

      , testCase "lookup convert" $ do
          lookupBuiltin "convert" @?= Just BConvert

      , testCase "lookup sumQty" $ do
          lookupBuiltin "sumQty" @?= Just BSumQty

      , testCase "lookup sumDec" $ do
          lookupBuiltin "sumDec" @?= Just BSumDec

      , testCase "lookup map" $ do
          lookupBuiltin "map" @?= Just BMap

      , testCase "lookup filter" $ do
          lookupBuiltin "filter" @?= Just BFilter

      , testCase "lookup fold" $ do
          lookupBuiltin "fold" @?= Just BFold

      , testCase "lookup emitCompliance" $ do
          lookupBuiltin "emitCompliance" @?= Just BEmitCompliance

      , testCase "lookup unknown returns Nothing" $ do
          lookupBuiltin "unknownFunc" @?= Nothing
      ]

  , testGroup "Builtin Signatures"
      [ testCase "getFact has arity 2" $ do
          sigArity (builtinSignature BGetFact) @?= 2

      , testCase "getFactsByPrefix has arity 2" $ do
          sigArity (builtinSignature BGetFactsByPrefix) @?= 2

      , testCase "isSome has arity 1" $ do
          sigArity (builtinSignature BIsSome) @?= 1

      , testCase "unwrapOr has arity 2" $ do
          sigArity (builtinSignature BUnwrapOr) @?= 2

      , testCase "requireSome has arity 3" $ do
          sigArity (builtinSignature BRequireSome) @?= 3

      , testCase "toDec has arity 2" $ do
          sigArity (builtinSignature BToDec) @?= 2

      , testCase "toQty has arity 2" $ do
          sigArity (builtinSignature BToQty) @?= 2

      , testCase "convert has arity 3" $ do
          sigArity (builtinSignature BConvert) @?= 3

      , testCase "sumQty has arity 2" $ do
          sigArity (builtinSignature BSumQty) @?= 2

      , testCase "sumDec has arity 2" $ do
          sigArity (builtinSignature BSumDec) @?= 2

      , testCase "map has arity 2" $ do
          sigArity (builtinSignature BMap) @?= 2

      , testCase "filter has arity 2" $ do
          sigArity (builtinSignature BFilter) @?= 2

      , testCase "fold has arity 3" $ do
          sigArity (builtinSignature BFold) @?= 3

      , testCase "emitCompliance has arity 3" $ do
          sigArity (builtinSignature BEmitCompliance) @?= 3
      ]

  , testGroup "All Builtins"
      [ testCase "allBuiltins has 16 entries" $ do
          length allBuiltins @?= 16

      , testCase "all builtins have names" $ do
          assertBool "all have non-empty names" $
            all (not . null . show) allBuiltins

      , testCase "all builtins have signatures" $ do
          assertBool "all have positive arity" $
            all (\b -> sigArity (builtinSignature b) >= 0) allBuiltins
      ]
  ]
