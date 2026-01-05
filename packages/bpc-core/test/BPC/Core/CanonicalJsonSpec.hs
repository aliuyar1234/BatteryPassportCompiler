{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.CanonicalJsonSpec (tests) where

import BPC.Core.CanonicalJson
import BPC.Core.Error
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "BPC.Core.CanonicalJson"
  [ testCase "sorts object keys" $
      canonicalEncode (object ["b" .= (2 :: Int), "a" .= (1 :: Int)])
        @?= Right "{\"a\":1,\"b\":2}"

  , testCase "rejects floats" $
      case canonicalEncode (Number 1.5) of
        Left (CanonicalNumberNotAllowed _) -> pure ()
        other -> assertFailure $ "Expected CanonicalNumberNotAllowed, got: " ++ show other

  , testCase "encodes empty object" $
      canonicalEncode (Object KM.empty) @?= Right "{}"

  , testCase "encodes empty array" $
      canonicalEncode (Array mempty) @?= Right "[]"

  , testCase "encodes nested objects" $
      canonicalEncode (object ["outer" .= object ["inner" .= (1 :: Int)]])
        @?= Right "{\"outer\":{\"inner\":1}}"

  , testCase "encodes unicode strings" $
      canonicalEncode (String "hello \x00e9") @?= Right "\"hello \195\169\""

  , testProperty "deterministic encoding" prop_deterministic
  ]

prop_deterministic :: Value -> Property
prop_deterministic val =
  case (canonicalEncode val, canonicalEncode val) of
    (Right a, Right b) -> a === b
    (Left _, Left _) -> property True
    _ -> property False

instance Arbitrary Value where
  arbitrary = sized genValue
    where
      genValue 0 = oneof [genNull, genBool, genInt, genString]
      genValue n = oneof
        [ genNull
        , genBool
        , genInt
        , genString
        , genArray (n `div` 2)
        , genObject (n `div` 2)
        ]

      genNull = pure Null
      genBool = Bool <$> arbitrary
      genInt = Number . fromInteger <$> arbitrary
      genString = String . pack <$> listOf (elements ['a'..'z'])
      genArray n = Array . fromList <$> resize 3 (listOf (genValue (n `div` 2)))
      genObject n = do
        keys <- resize 3 $ listOf1 $ listOf1 (elements ['a'..'z'])
        vals <- vectorOf (length keys) (genValue (n `div` 2))
        pure $ object $ zipWith (\k v -> pack k .= v) keys vals

      pack = id
      fromList = foldr (\x acc -> acc `snoc` x) mempty
      snoc xs x = xs <> pure x
