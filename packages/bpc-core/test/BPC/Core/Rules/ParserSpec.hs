{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.Rules.ParserSpec (tests) where

import Data.Either (isLeft, isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import BPC.Core.Rules.AST
import BPC.Core.Rules.Parser (parseSource)

tests :: TestTree
tests = testGroup "BPC.Core.Rules.Parser"
  [ testGroup "Field Declarations"
      [ testCase "parse simple field declaration" $ do
          let src = "field battery.capacity_kwh: Dec(6) = toDec(6, 100);"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse field with let expression" $ do
          let src = "field battery.total: Dec(6) = let x = 10; toDec(6, x);"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse field with type annotation Dec" $ do
          let src = "field f: Dec(6) = toDec(6, 1);"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldType fd @?= TDec 6
            _ -> fail "Expected field declaration"

      , testCase "parse field with type annotation Qty" $ do
          let src = "field f: Qty(\"kg\") = toQty(\"kg\", 100);"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldType fd @?= TQty "kg"
            _ -> fail "Expected field declaration"
      ]

  , testGroup "Let Expressions"
      [ testCase "parse simple let" $ do
          let src = "field f: Int = let x = 1; x;"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse nested let" $ do
          let src = "field f: Int = let x = 1; let y = 2; x + y;"
          assertBool "should parse" (isRight $ parseSource src)
      ]

  , testGroup "If/Then/Else"
      [ testCase "parse if expression" $ do
          let src = "field f: Int = if true then 1 else 0;"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse nested if" $ do
          let src = "field f: Int = if true then if false then 1 else 2 else 3;"
          assertBool "should parse" (isRight $ parseSource src)
      ]

  , testGroup "Comments"
      [ testCase "ignore line comment" $ do
          let src = "-- this is a comment\nfield f: Int = 1;"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "ignore inline comment" $ do
          let src = "field f: Int = 1; -- inline"
          assertBool "should parse" (isRight $ parseSource src)
      ]

  , testGroup "Operators"
      [ testCase "parse arithmetic operators" $ do
          let src = "field f: Int = 1 + 2 * 3 - 4 / 2;"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse comparison operators" $ do
          let src = "field f: Bool = 1 < 2 && 3 > 1 || 4 == 4;"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "operator precedence: * before +" $ do
          let src = "field f: Int = 1 + 2 * 3;"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              case fieldExpr fd of
                UBinOp "+" _ (UBinOp "*" _ _) -> pure ()
                _ -> fail "Expected + with * on right"
            _ -> fail "Parse failed"
      ]

  , testGroup "Function Calls"
      [ testCase "parse function with no args" $ do
          let src = "field f: Bool = someFunc();"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse function with multiple args" $ do
          let src = "field f: Dec(6) = toDec(6, 100);"
          assertBool "should parse" (isRight $ parseSource src)

      , testCase "parse nested function calls" $ do
          let src = "field f: Int = foo(bar(1), baz(2, 3));"
          assertBool "should parse" (isRight $ parseSource src)
      ]

  , testGroup "Syntax Errors"
      [ testCase "missing semicolon returns error" $ do
          let src = "field f: Int = 1"
          assertBool "should fail" (isLeft $ parseSource src)

      , testCase "missing type annotation returns error" $ do
          let src = "field f = 1;"
          assertBool "should fail" (isLeft $ parseSource src)

      , testCase "unclosed paren returns error" $ do
          let src = "field f: Int = foo(1;"
          assertBool "should fail" (isLeft $ parseSource src)
      ]

  , testGroup "Literals"
      [ testCase "parse boolean true" $ do
          let src = "field f: Bool = true;"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldExpr fd @?= ULitBool True
            _ -> fail "Parse failed"

      , testCase "parse boolean false" $ do
          let src = "field f: Bool = false;"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldExpr fd @?= ULitBool False
            _ -> fail "Parse failed"

      , testCase "parse integer" $ do
          let src = "field f: Int = 42;"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldExpr fd @?= ULitInt 42
            _ -> fail "Parse failed"

      , testCase "parse string" $ do
          let src = "field f: String = \"hello\";"
          case parseSource src of
            Right (Module [DeclField fd]) ->
              fieldExpr fd @?= ULitString "hello"
            _ -> fail "Parse failed"

      , testCase "parse decimal" $ do
          let src = "field f: Dec(2) = 3.14;"
          assertBool "should parse" (isRight $ parseSource src)
      ]

  , testGroup "Assert"
      [ testCase "parse assert expression" $ do
          let src = "field f: Bool = assert(x > 0, \"E001\", \"must be positive\");"
          assertBool "should parse" (isRight $ parseSource src)
      ]
  ]
