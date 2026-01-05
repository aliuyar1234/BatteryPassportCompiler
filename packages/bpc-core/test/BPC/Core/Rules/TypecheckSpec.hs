{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.Rules.TypecheckSpec (tests) where

import Data.Either (isLeft, isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import BPC.Core.Rules.AST
import BPC.Core.Rules.Error
import BPC.Core.Rules.Parser (parseSource)
import BPC.Core.Rules.Typecheck

tests :: TestTree
tests = testGroup "BPC.Core.Rules.Typecheck"
  [ testGroup "Literals"
      [ testCase "typecheck boolean literal" $ do
          let expr = ULitBool True
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck integer literal" $ do
          let expr = ULitInt 42
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck string literal" $ do
          let expr = ULitString "hello"
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)
      ]

  , testGroup "Binary Operators"
      [ testCase "typecheck int + int = int" $ do
          let expr = UBinOp "+" (ULitInt 1) (ULitInt 2)
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck bool && bool = bool" $ do
          let expr = UBinOp "&&" (ULitBool True) (ULitBool False)
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck comparison returns bool" $ do
          let expr = UBinOp "==" (ULitInt 1) (ULitInt 1)
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)
      ]

  , testGroup "If/Then/Else"
      [ testCase "typecheck valid if expression" $ do
          let expr = UIf (ULitBool True) (ULitInt 1) (ULitInt 2)
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "reject non-bool condition" $ do
          let expr = UIf (ULitInt 1) (ULitInt 1) (ULitInt 2)
          case typecheckExpr emptyTypeEnv emptyFieldEnv expr of
            Left (TypeMismatch "Bool" _ _) -> pure ()
            _ -> fail "Expected type mismatch error"
      ]

  , testGroup "Let Expressions"
      [ testCase "typecheck let binding" $ do
          let expr = ULet (Identifier "x") (ULitInt 1) (UVar (Identifier "x"))
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)
      ]

  , testGroup "Function Calls"
      [ testCase "typecheck getFact" $ do
          let expr = UFuncCall (Identifier "getFact") [ULitString "Battery", ULitString "key"]
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck isSome" $ do
          let expr = UFuncCall (Identifier "isSome") [ULitString "x"]
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "typecheck toDec" $ do
          let expr = UFuncCall (Identifier "toDec") [ULitInt 6, ULitInt 100]
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)
      ]

  , testGroup "Type Errors"
      [ testCase "detect unknown field" $ do
          let expr = UFieldRef (FieldPath "unknown.field")
          case typecheckExpr emptyTypeEnv emptyFieldEnv expr of
            Left (UnknownField _) -> pure ()
            _ -> fail "Expected unknown field error"

      , testCase "detect unknown variable" $ do
          let expr = UVar (Identifier "unknown")
          case typecheckExpr emptyTypeEnv emptyFieldEnv expr of
            Left (UnknownField _) -> pure ()
            _ -> fail "Expected unknown field error"
      ]

  , testGroup "Assert"
      [ testCase "typecheck assert with bool condition" $ do
          let expr = UAssert (ULitBool True) "E001" "message"
          assertBool "should typecheck" (isRight $ typecheckExpr emptyTypeEnv emptyFieldEnv expr)

      , testCase "reject assert with non-bool condition" $ do
          let expr = UAssert (ULitInt 1) "E001" "message"
          case typecheckExpr emptyTypeEnv emptyFieldEnv expr of
            Left (TypeMismatch "Bool" _ _) -> pure ()
            _ -> fail "Expected type mismatch error"
      ]

  , testGroup "Module Typecheck"
      [ testCase "typecheck simple module" $ do
          let src = "field f: Int = 42;"
          case parseSource src of
            Right modul -> assertBool "should typecheck" (isRight $ typecheckSource modul)
            Left _ -> fail "Parse failed"

      , testCase "typecheck module with multiple fields" $ do
          let src = "field a: Int = 1; field b: Int = 2;"
          case parseSource src of
            Right modul -> assertBool "should typecheck" (isRight $ typecheckSource modul)
            Left _ -> fail "Parse failed"
      ]
  ]
