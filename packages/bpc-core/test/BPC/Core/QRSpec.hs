{-# LANGUAGE OverloadedStrings #-}

module BPC.Core.QRSpec (tests) where

import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import BPC.Core.QR

tests :: TestTree
tests = testGroup "BPC.Core.QR"
  [ testGroup "QR Payload Building"
      [ testCase "build QR payload from input" $ do
          let payload = buildQrPayload minimalQrInput
          qpVersion payload @?= "BPC1"

      , testCase "version is BPC1" $ do
          let payload = buildQrPayload minimalQrInput
          qpVersion payload @?= "BPC1"

      , testCase "passport version ID preserved" $ do
          let payload = buildQrPayload minimalQrInput
          qpPassportVersionId payload @?= qiPassportVersionId minimalQrInput
      ]

  , testGroup "QR String Formatting"
      [ testCase "format matches BPC-QR-1" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should start with BPC1" (T.isPrefixOf "BPC1|" qrString)

      , testCase "format has 5 pipe-separated parts" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          let parts = T.splitOn "|" qrString
          length parts @?= 5

      , testCase "format includes pv field" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should contain pv=" (T.isInfixOf "pv=" qrString)

      , testCase "format includes ph field" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should contain ph=" (T.isInfixOf "ph=" qrString)

      , testCase "format includes pr field" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should contain pr=" (T.isInfixOf "pr=" qrString)

      , testCase "format includes rh field" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should contain rh=" (T.isInfixOf "rh=" qrString)
      ]

  , testGroup "Base32 Encoding"
      [ testCase "Base32 contains only [A-Z2-7]" $ do
          let payload = buildQrPayload minimalQrInput
          assertBool "valid Base32" (isValidBase32Chars $ qpPayloadHashB32 payload)

      , testCase "Base32 has no padding" $ do
          let payload = buildQrPayload minimalQrInput
          assertBool "no padding" (not $ T.isInfixOf "=" $ qpPayloadHashB32 payload)
      ]

  , testGroup "QR Parsing"
      [ testCase "parse valid QR string" $ do
          let payload = buildQrPayload minimalQrInput
          let qrString = formatQrString payload
          assertBool "should parse" (isRight $ parseQrPayload qrString)

      , testCase "roundtrip: build -> format -> parse" $ do
          let original = buildQrPayload minimalQrInput
          let qrString = formatQrString original
          case parseQrPayload qrString of
            Right parsed -> do
              qpVersion parsed @?= qpVersion original
              qpPassportVersionId parsed @?= qpPassportVersionId original
            Left err -> fail $ "Parse failed: " <> T.unpack err
      ]

  , testGroup "Property Tests"
      [ testProperty "prop_qr_deterministic" $ \n ->
          let input = minimalQrInput { qiPayloadHash = padHex n }
              payload1 = buildQrPayload input
              payload2 = buildQrPayload input
          in formatQrString payload1 == formatQrString payload2
      ]
  ]

-- Minimal QR input for testing
minimalQrInput :: QrInput
minimalQrInput = QrInput
  { qiPassportVersionId = UUID.nil
  , qiPayloadHash = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  , qiProofHash = "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
  , qiReceiptHash = "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
  }

-- Check if text contains only valid Base32 characters
isValidBase32Chars :: T.Text -> Bool
isValidBase32Chars = T.all isBase32Char
  where
    isBase32Char c = (c >= 'A' && c <= 'Z') || (c >= '2' && c <= '7')

-- Pad an integer to a 64-char hex string
padHex :: Int -> T.Text
padHex n = T.pack $ take 64 $ cycle $ show (abs n) ++ "abcdef0123456789"
