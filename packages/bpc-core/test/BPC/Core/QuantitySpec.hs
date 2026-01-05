{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BPC.Core.QuantitySpec (tests) where

import BPC.Core.Error
import BPC.Core.Types.Decimal
import BPC.Core.Types.Quantity
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "BPC.Core.Types.Quantity"
  [ testGroup "Construction"
      [ testCase "mkQty with valid unit" $
          case mkQty "kg" (toDec @6 1000000) of
            Right q -> qtyUnit q @?= "kg"
            Left _ -> assertFailure "Expected success"

      , testCase "mkQty with invalid unit" $
          case mkQty "invalid" (toDec @6 1000000) of
            Left (UnknownUnit _) -> pure ()
            other -> assertFailure $ "Expected UnknownUnit, got: " ++ show other
      ]

  , testGroup "Conversion"
      [ testCase "kg to g conversion" $
          let q = Qty (toDec @6 1000000) "kg"  -- 1 kg
           in case convertQty q "g" of
                Right result -> qtyUnit result @?= "g"
                Left e -> assertFailure $ "Conversion failed: " ++ show e

      , testCase "incompatible units fail" $
          let q = Qty (toDec @6 1000000) "kg"
           in case convertQty q "kWh" of
                Left (IncompatibleUnits _ _) -> pure ()
                other -> assertFailure $ "Expected IncompatibleUnits, got: " ++ show other
      ]

  , testGroup "Arithmetic"
      [ testCase "same unit addition works" $
          let q1 = Qty (toDec @6 1000000) "kg"
              q2 = Qty (toDec @6 2000000) "kg"
           in case addQty q1 q2 of
                Right result -> unDec (qtyValue result) @?= 3000000
                Left e -> assertFailure $ "Addition failed: " ++ show e

      , testCase "different unit addition fails" $
          let q1 = Qty (toDec @6 1000000) "kg"
              q2 = Qty (toDec @6 2000000) "g"
           in case addQty q1 q2 of
                Left (UnitMismatch _ _) -> pure ()
                other -> assertFailure $ "Expected UnitMismatch, got: " ++ show other

      , testCase "multiplication by scalar" $
          let q = Qty (toDec @6 2000000) "kg"
              factor = toDec @6 3000000
           in unDec (qtyValue (mulQtyDec q factor)) @?= 6000000

      , testCase "division by zero fails" $
          let q = Qty (toDec @6 1000000) "kg"
              divisor = toDec @6 0
           in case divQtyDec q divisor of
                Left DivisionByZero -> pure ()
                other -> assertFailure $ "Expected DivisionByZero, got: " ++ show other
      ]

  , testGroup "Aggregation"
      [ testCase "sumQty works with same units" $
          let quantities =
                [ Qty (toDec @6 1000000) "kg"
                , Qty (toDec @6 2000000) "kg"
                , Qty (toDec @6 3000000) "kg"
                ]
           in case sumQty quantities of
                Right result -> unDec (qtyValue result) @?= 6000000
                Left e -> assertFailure $ "Sum failed: " ++ show e
      ]
  ]
