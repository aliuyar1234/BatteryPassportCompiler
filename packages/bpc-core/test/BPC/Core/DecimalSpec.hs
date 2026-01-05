{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module BPC.Core.DecimalSpec (tests) where

import BPC.Core.Types.Decimal
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "BPC.Core.Types.Decimal"
  [ testGroup "Construction"
      [ testCase "toDec creates Dec" $
          unDec (toDec @6 1000000) @?= 1000000

      , testCase "fromInteger' scales correctly" $
          unDec (fromInteger' @6 5) @?= 5000000

      , testCase "zero is zero" $
          unDec (zero @6) @?= 0
      ]

  , testGroup "Arithmetic"
      [ testCase "addition works" $
          let a = toDec @6 1000000  -- 1.0
              b = toDec @6 2000000  -- 2.0
           in unDec (addDec a b) @?= 3000000  -- 3.0

      , testCase "subtraction works" $
          let a = toDec @6 3000000  -- 3.0
              b = toDec @6 1000000  -- 1.0
           in unDec (subDec a b) @?= 2000000  -- 2.0

      , testCase "multiplication works" $
          let a = toDec @6 2000000  -- 2.0
              b = toDec @6 3000000  -- 3.0
           in unDec (mulDec a b) @?= 6000000  -- 6.0

      , testCase "division works" $
          let a = toDec @6 6000000  -- 6.0
              b = toDec @6 2000000  -- 2.0
           in fmap unDec (divDec a b) @?= Just 3000000  -- 3.0

      , testCase "division by zero returns Nothing" $
          divDec (toDec @6 1000000) (toDec @6 0) @?= Nothing
      ]

  , testGroup "Properties"
      [ testProperty "addition is commutative" $ \(a :: Integer) (b :: Integer) ->
          let da = toDec @6 a
              db = toDec @6 b
           in addDec da db == addDec db da

      , testProperty "zero is identity for addition" $ \(a :: Integer) ->
          let da = toDec @6 a
           in addDec da zero == da
      ]
  ]
