{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.Rules.GraphSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Either (isLeft, isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.Rules.AST
import BPC.Core.Rules.Graph

tests :: TestTree
tests = testGroup "BPC.Core.Rules.Graph"
  [ testGroup "Extract Dependencies"
      [ testCase "literal has no deps" $ do
          extractDeps (ULitInt 1) @?= Set.empty

      , testCase "field ref has one dep" $ do
          extractDeps (UFieldRef (FieldPath "a.b")) @?= Set.singleton "a.b"

      , testCase "binary op combines deps" $ do
          let expr = UBinOp "+" (UFieldRef (FieldPath "a")) (UFieldRef (FieldPath "b"))
          extractDeps expr @?= Set.fromList ["a", "b"]

      , testCase "let combines value and body deps" $ do
          let expr = ULet (Identifier "x") (UFieldRef (FieldPath "a")) (UFieldRef (FieldPath "b"))
          extractDeps expr @?= Set.fromList ["a", "b"]

      , testCase "function call combines arg deps" $ do
          let expr = UFuncCall (Identifier "f") [UFieldRef (FieldPath "a"), UFieldRef (FieldPath "b")]
          extractDeps expr @?= Set.fromList ["a", "b"]
      ]

  , testGroup "Build Graph"
      [ testCase "empty module has empty graph" $ do
          let graph = buildGraph (Module [])
          dgNodes graph @?= Set.empty

      , testCase "single field with no deps" $ do
          let decl = DeclField $ FieldDecl (FieldPath "a") TInt (ULitInt 1)
          let graph = buildGraph (Module [decl])
          dgNodes graph @?= Set.singleton "a"
          Map.lookup "a" (dgEdges graph) @?= Just Set.empty

      , testCase "field with dependency" $ do
          let declA = DeclField $ FieldDecl (FieldPath "a") TInt (ULitInt 1)
          let declB = DeclField $ FieldDecl (FieldPath "b") TInt (UFieldRef (FieldPath "a"))
          let graph = buildGraph (Module [declA, declB])
          dgNodes graph @?= Set.fromList ["a", "b"]
          Map.lookup "b" (dgEdges graph) @?= Just (Set.singleton "a")
      ]

  , testGroup "Topological Sort"
      [ testCase "empty graph returns empty list" $ do
          let result = topoSort Set.empty Map.empty
          result @?= Right []

      , testCase "single node returns that node" $ do
          let result = topoSort (Set.singleton "a") (Map.singleton "a" Set.empty)
          result @?= Right ["a"]

      , testCase "dependency comes before dependent" $ do
          let nodes = Set.fromList ["a", "b"]
          let edges = Map.fromList [("a", Set.empty), ("b", Set.singleton "a")]
          case topoSort nodes edges of
            Right order -> do
              let aIdx = elemIndex' "a" order
              let bIdx = elemIndex' "b" order
              assertBool "a before b" (aIdx < bIdx)
            Left _ -> fail "Expected success"

      , testCase "chain: c depends on b depends on a" $ do
          let nodes = Set.fromList ["a", "b", "c"]
          let edges = Map.fromList
                [ ("a", Set.empty)
                , ("b", Set.singleton "a")
                , ("c", Set.singleton "b")
                ]
          case topoSort nodes edges of
            Right order -> do
              let aIdx = elemIndex' "a" order
              let bIdx = elemIndex' "b" order
              let cIdx = elemIndex' "c" order
              assertBool "a before b" (aIdx < bIdx)
              assertBool "b before c" (bIdx < cIdx)
            Left _ -> fail "Expected success"

      , testCase "independent nodes sorted lexically" $ do
          let nodes = Set.fromList ["c", "a", "b"]
          let edges = Map.fromList [("a", Set.empty), ("b", Set.empty), ("c", Set.empty)]
          case topoSort nodes edges of
            Right order -> order @?= ["a", "b", "c"]
            Left _ -> fail "Expected success"
      ]

  , testGroup "Cycle Detection"
      [ testCase "detect simple cycle A -> B -> A" $ do
          let nodes = Set.fromList ["a", "b"]
          let edges = Map.fromList [("a", Set.singleton "b"), ("b", Set.singleton "a")]
          assertBool "should detect cycle" (isLeft $ topoSort nodes edges)

      , testCase "detect complex cycle A -> B -> C -> D -> B" $ do
          let nodes = Set.fromList ["a", "b", "c", "d"]
          let edges = Map.fromList
                [ ("a", Set.singleton "b")
                , ("b", Set.singleton "c")
                , ("c", Set.singleton "d")
                , ("d", Set.singleton "b")
                ]
          assertBool "should detect cycle" (isLeft $ topoSort nodes edges)

      , testCase "no false positive on DAG" $ do
          let nodes = Set.fromList ["a", "b", "c"]
          let edges = Map.fromList
                [ ("a", Set.empty)
                , ("b", Set.singleton "a")
                , ("c", Set.fromList ["a", "b"])
                ]
          assertBool "should succeed" (isRight $ topoSort nodes edges)
      ]

  , testGroup "Property Tests"
      [ testProperty "all deps come before node" $ \n ->
          n >= 0 && n <= 10 ==>
            let nodes = Set.fromList [show i | i <- [0..n]]
                edges = Map.fromList [(show i, Set.fromList [show j | j <- [0..i-1]]) | i <- [0..n]]
            in case topoSort nodes edges of
                 Right order ->
                   all (\i -> all (\j -> elemIndex' (show j) order < elemIndex' (show i) order) [0..i-1]) [0..n]
                 Left _ -> False
      ]
  ]

-- Helper to find index
elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x xs = go 0 xs
  where
    go _ [] = -1
    go i (y:ys)
      | x == y = i
      | otherwise = go (i + 1) ys

-- QuickCheck import
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> b = b
