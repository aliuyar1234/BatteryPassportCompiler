{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.Rules.EvalSpec (tests) where

import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.Rules.AST
import BPC.Core.Rules.Error
import BPC.Core.Rules.Eval

tests :: TestTree
tests = testGroup "BPC.Core.Rules.Eval"
  [ testGroup "Literal Evaluation"
      [ testCase "eval bool true" $ do
          evalExpr emptyContext (ULitBool True) @?= Right (VBool True)

      , testCase "eval bool false" $ do
          evalExpr emptyContext (ULitBool False) @?= Right (VBool False)

      , testCase "eval integer" $ do
          evalExpr emptyContext (ULitInt 42) @?= Right (VInt 42)

      , testCase "eval string" $ do
          evalExpr emptyContext (ULitString "hello") @?= Right (VString "hello")

      , testCase "eval decimal" $ do
          evalExpr emptyContext (ULitDec 2 314) @?= Right (VDec 2 314)
      ]

  , testGroup "Binary Operators"
      [ testCase "eval int addition" $ do
          let expr = UBinOp "+" (ULitInt 1) (ULitInt 2)
          evalExpr emptyContext expr @?= Right (VInt 3)

      , testCase "eval int subtraction" $ do
          let expr = UBinOp "-" (ULitInt 5) (ULitInt 3)
          evalExpr emptyContext expr @?= Right (VInt 2)

      , testCase "eval int multiplication" $ do
          let expr = UBinOp "*" (ULitInt 4) (ULitInt 3)
          evalExpr emptyContext expr @?= Right (VInt 12)

      , testCase "eval int division" $ do
          let expr = UBinOp "/" (ULitInt 10) (ULitInt 3)
          evalExpr emptyContext expr @?= Right (VInt 3)

      , testCase "eval boolean and" $ do
          let expr = UBinOp "&&" (ULitBool True) (ULitBool False)
          evalExpr emptyContext expr @?= Right (VBool False)

      , testCase "eval boolean or" $ do
          let expr = UBinOp "||" (ULitBool False) (ULitBool True)
          evalExpr emptyContext expr @?= Right (VBool True)

      , testCase "eval equality" $ do
          let expr = UBinOp "==" (ULitInt 1) (ULitInt 1)
          evalExpr emptyContext expr @?= Right (VBool True)

      , testCase "eval inequality" $ do
          let expr = UBinOp "!=" (ULitInt 1) (ULitInt 2)
          evalExpr emptyContext expr @?= Right (VBool True)

      , testCase "eval less than" $ do
          let expr = UBinOp "<" (ULitInt 1) (ULitInt 2)
          evalExpr emptyContext expr @?= Right (VBool True)

      , testCase "eval greater than" $ do
          let expr = UBinOp ">" (ULitInt 2) (ULitInt 1)
          evalExpr emptyContext expr @?= Right (VBool True)
      ]

  , testGroup "Let Expressions"
      [ testCase "eval simple let" $ do
          let expr = ULet (Identifier "x") (ULitInt 1) (UVar (Identifier "x"))
          evalExpr emptyContext expr @?= Right (VInt 1)

      , testCase "eval nested let" $ do
          let expr = ULet (Identifier "x") (ULitInt 1)
                       (ULet (Identifier "y") (ULitInt 2)
                         (UBinOp "+" (UVar (Identifier "x")) (UVar (Identifier "y"))))
          evalExpr emptyContext expr @?= Right (VInt 3)

      , testCase "eval let shadowing" $ do
          let expr = ULet (Identifier "x") (ULitInt 1)
                       (ULet (Identifier "x") (ULitInt 2)
                         (UVar (Identifier "x")))
          evalExpr emptyContext expr @?= Right (VInt 2)
      ]

  , testGroup "If/Then/Else"
      [ testCase "eval if true branch" $ do
          let expr = UIf (ULitBool True) (ULitInt 1) (ULitInt 2)
          evalExpr emptyContext expr @?= Right (VInt 1)

      , testCase "eval if false branch" $ do
          let expr = UIf (ULitBool False) (ULitInt 1) (ULitInt 2)
          evalExpr emptyContext expr @?= Right (VInt 2)

      , testCase "eval nested if" $ do
          let expr = UIf (ULitBool True)
                       (UIf (ULitBool False) (ULitInt 1) (ULitInt 2))
                       (ULitInt 3)
          evalExpr emptyContext expr @?= Right (VInt 2)
      ]

  , testGroup "Field References"
      [ testCase "eval field reference" $ do
          let ctx = emptyContext { ctxFields = Map.singleton "a.b" (VInt 42) }
          evalExpr ctx (UFieldRef (FieldPath "a.b")) @?= Right (VInt 42)

      , testCase "unknown field returns error" $ do
          assertBool "should fail" (isLeft $ evalExpr emptyContext (UFieldRef (FieldPath "unknown")))
      ]

  , testGroup "Error Cases"
      [ testCase "division by zero" $ do
          let expr = UBinOp "/" (ULitInt 1) (ULitInt 0)
          case evalExpr emptyContext expr of
            Left (DivisionByZero _) -> pure ()
            _ -> fail "Expected division by zero error"

      , testCase "unknown variable" $ do
          let expr = UVar (Identifier "unknown")
          assertBool "should fail" (isLeft $ evalExpr emptyContext expr)
      ]

  , testGroup "Assert"
      [ testCase "assert true succeeds" $ do
          let expr = UAssert (ULitBool True) "E001" "message"
          evalExpr emptyContext expr @?= Right (VBool True)

      , testCase "assert false fails" $ do
          let expr = UAssert (ULitBool False) "E001" "must be true"
          case evalExpr emptyContext expr of
            Left (AssertionFailed "must be true" "E001") -> pure ()
            _ -> fail "Expected assertion failed error"
      ]

  , testGroup "Built-in Functions"
      [ testCase "eval isSome with none" $ do
          let ctx = emptyContext
          let expr = UFuncCall (Identifier "isSome") [UFuncCall (Identifier "getFact") [ULitString "X", ULitString "missing"]]
          evalExpr ctx expr @?= Right (VBool False)

      , testCase "eval unwrapOr with default" $ do
          let expr = UFuncCall (Identifier "unwrapOr")
                       [UFuncCall (Identifier "getFact") [ULitString "X", ULitString "missing"], ULitString "default"]
          case evalExpr emptyContext expr of
            Right (VString "default") -> pure ()
            _ -> fail "Expected default value"

      , testCase "eval toDec" $ do
          let expr = UFuncCall (Identifier "toDec") [ULitInt 6, ULitInt 100]
          evalExpr emptyContext expr @?= Right (VDec 6 100)

      , testCase "eval toQty" $ do
          let expr = UFuncCall (Identifier "toQty") [ULitString "kg", ULitInt 100]
          evalExpr emptyContext expr @?= Right (VQty 6 100 "kg")
      ]

  , testGroup "Determinism"
      [ testProperty "evaluation is deterministic" $ \n ->
          let expr = ULitInt (fromIntegral (n :: Int))
              r1 = evalExpr emptyContext expr
              r2 = evalExpr emptyContext expr
          in r1 == r2
      ]
  ]
