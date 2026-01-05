# Quickstart: Core Primitives

**Feature**: 02-core-primitives
**Date**: 2025-12-28
**Package**: bpc-core

## Prerequisites

- 01-foundation complete (build environment working)
- GHC 9.6.4, Cabal 3.10.2.1 installed
- Basic familiarity with Haskell type-level programming (DataKinds)

## Installation

```bash
# Navigate to project root
cd D:\Projekte\BatteryPassportCompiler

# Build bpc-core
cabal build bpc-core

# Run tests
cabal test bpc-core

# Generate documentation
cabal haddock bpc-core
```

## Quick Usage Examples

### 1. Canonical JSON Encoding

```haskell
{-# LANGUAGE OverloadedStrings #-}

import BPC.Core.CanonicalJson
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS

-- Example 1: Basic encoding
example1 :: IO ()
example1 = do
  let json = object ["b" .= (2 :: Int), "a" .= (1 :: Int)]
  case canonicalEncode json of
    Left err -> putStrLn $ "Error: " ++ show err
    Right bytes -> BS.putStr bytes
    -- Output: {"a":1,"b":2}

-- Example 2: Float rejection
example2 :: IO ()
example2 = do
  let badJson = object ["x" .= (1.5 :: Double)]
  case canonicalEncode badJson of
    Left (CanonicalNumberNotAllowed n) ->
      putStrLn $ "Rejected float: " ++ show n
    Right _ -> putStrLn "Unexpected success"

-- Example 3: Round-trip
example3 :: IO ()
example3 = do
  let json = object ["hello" .= ("world" :: String)]
  case canonicalEncode json of
    Left err -> print err
    Right bytes ->
      case canonicalDecode bytes of
        Left err -> print err
        Right decoded ->
          print (decoded == json)  -- True
```

### 2. Cryptographic Hashing

```haskell
import BPC.Core.Hash
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Example 1: SHA-256 hashing
hashExample :: IO ()
hashExample = do
  let canonical = "{\"a\":1,\"b\":2}"
  let hash = sha256Hex canonical
  T.putStrLn hash
  -- Output: 43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777

-- Example 2: Base32 encoding for QR
qrExample :: IO ()
qrExample = do
  let hash = sha256Bytes "hello"
  let encoded = base32NoPad hash
  T.putStrLn encoded
  -- Output: NBSWY3DPEB3W64TMMQ (no padding)

-- Example 3: Hash and encode pipeline
hashAndEncode :: ByteString -> Text
hashAndEncode input =
  let hash = sha256Bytes input
      encoded = base32NoPad hash
  in encoded
```

### 3. Type-Safe Domain IDs

```haskell
import BPC.Core.Types.Domain
import Data.UUID (nil)
import Data.UUID.V4 (nextRandom)

-- Example 1: Creating IDs
createIds :: IO (TenantId, PassportId)
createIds = do
  tenantUuid <- nextRandom
  passportUuid <- nextRandom
  pure (TenantId tenantUuid, PassportId passportUuid)

-- Example 2: Type safety in action
typeExample :: IO ()
typeExample = do
  (tenantId, passportId) <- createIds

  -- This works:
  let tid = tenantId

  -- This does NOT compile:
  -- let confusion :: TenantId = passportId  -- TYPE ERROR!

  -- Must explicitly unwrap:
  let uuid = unPassportId passportId
  print uuid

-- Example 3: JSON serialization
jsonExample :: IO ()
jsonExample = do
  let tid = TenantId nil
  let json = toJSON tid
  print json  -- String "00000000-0000-0000-0000-000000000000"
```

### 4. Decimal Arithmetic

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import BPC.Core.Types.Decimal
import Data.Text.IO as T

-- Example 1: Basic arithmetic
decExample1 :: IO ()
decExample1 = do
  let price :: Dec 2 = Dec 1299  -- $12.99
  let tax :: Dec 2 = Dec 78      -- $0.78
  let total = price + tax         -- Dec 1377 = $13.77
  T.putStrLn $ renderDec total    -- "13.77"

-- Example 2: Scale conversion
decExample2 :: IO ()
decExample2 = do
  let lowPrecision :: Dec 2 = Dec 150  -- 1.50
  let highPrecision = rescale @2 @6 lowPrecision  -- Dec 1500000 = 1.500000
  T.putStrLn $ renderDec highPrecision  -- "1.500000"

-- Example 3: Parsing from text
decExample3 :: IO ()
decExample3 = do
  case parseDec @6 "75.500000" of
    Left err -> print err
    Right (d :: Dec 6) -> do
      T.putStrLn $ renderDec d     -- "75.500000"
      print $ unDec d               -- 75500000

-- Example 4: Using Num instance
decExample4 :: IO ()
decExample4 = do
  let a :: Dec 6 = 10   -- fromInteger: Dec 10000000
  let b :: Dec 6 = 5    -- fromInteger: Dec 5000000
  let sum = a + b        -- Dec 15000000
  T.putStrLn $ renderDec sum  -- "15.000000"
```

### 5. Quantity Arithmetic

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import BPC.Core.Types.Quantity
import BPC.Core.Types.Decimal
import Data.Text.IO as T

-- Example 1: Creating quantities
qtyExample1 :: IO ()
qtyExample1 = do
  let mass :: Qty "kg" = Qty (Dec 450250000)  -- 450.25 kg
  let energy :: Qty "kWh" = Qty (Dec 75500000)  -- 75.5 kWh
  print mass
  print energy

-- Example 2: Same-unit operations
qtyExample2 :: IO ()
qtyExample2 = do
  let kg1 :: Qty "kg" = Qty (Dec 100000000)  -- 100 kg
  let kg2 :: Qty "kg" = Qty (Dec 50000000)   -- 50 kg
  let total = addQty kg1 kg2                  -- 150 kg
  let diff = subQty kg1 kg2                   -- 50 kg
  T.putStrLn $ renderDec (unQty total)        -- "150.000000"
  T.putStrLn $ renderDec (unQty diff)         -- "50.000000"

-- Example 3: Scalar multiplication
qtyExample3 :: IO ()
qtyExample3 = do
  let mass :: Qty "kg" = Qty (Dec 100000000)  -- 100 kg
  let factor :: Dec 6 = Dec 2000000            -- 2.0
  let doubled = mulQtyDec mass factor          -- 200 kg
  T.putStrLn $ renderDec (unQty doubled)       -- "200.000000"

-- Example 4: Type safety (these won't compile)
qtyExample4 :: IO ()
qtyExample4 = do
  let mass :: Qty "kg" = Qty (Dec 100000000)
  let energy :: Qty "kWh" = Qty (Dec 75500000)

  -- This DOES NOT compile:
  -- let invalid = addQty mass energy  -- TYPE ERROR: "kg" != "kWh"

  -- This DOES NOT compile:
  -- let wrongType :: Qty "g" = mass   -- TYPE ERROR: "g" != "kg"

  putStrLn "Type safety works!"
```

### 6. Unit Conversions

```haskell
import BPC.Core.Types.Units
import BPC.Core.Types.Decimal
import Data.Text.IO as T

-- Example 1: Mass conversion
unitExample1 :: IO ()
unitExample1 = do
  let grams = Dec @6 2000000000  -- 2000 g
  case convert G Kg grams of
    Left err -> print err
    Right kg -> T.putStrLn $ renderDec kg  -- "2.000000" kg

-- Example 2: Energy conversion
unitExample2 :: IO ()
unitExample2 = do
  let kwh = Dec @6 1500000  -- 1.5 kWh
  case convert KWh Wh kwh of
    Left err -> print err
    Right wh -> T.putStrLn $ renderDec wh  -- "1500.000000" Wh

-- Example 3: Incompatible units
unitExample3 :: IO ()
unitExample3 = do
  let mass = Dec @6 100000000  -- 100 kg
  case convert Kg KWh mass of
    Left (ConversionNotSupported from to) ->
      putStrLn $ "Cannot convert " ++ show from ++ " to " ++ show to
    Right _ -> putStrLn "Unexpected success"

-- Example 4: Parsing units from text
unitExample4 :: IO ()
unitExample4 = do
  case parseUnit "kg" of
    Nothing -> putStrLn "Unknown unit"
    Just unit -> T.putStrLn $ renderUnit unit  -- "kg"
```

## Complete Example: Hash a Struct for Storage

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import BPC.Core.CanonicalJson
import BPC.Core.Hash
import BPC.Core.Types.Domain
import Data.Aeson (ToJSON, toJSON, genericToJSON, defaultOptions)
import GHC.Generics (Generic)

-- Define a data structure
data Snapshot = Snapshot
  { snapshotId :: SnapshotId
  , facts :: [FactEntry]
  } deriving (Generic, Show)

data FactEntry = FactEntry
  { factType :: Text
  , factKey :: Text
  , payloadHash :: Text
  } deriving (Generic, Show)

instance ToJSON Snapshot where
  toJSON = genericToJSON defaultOptions

instance ToJSON FactEntry where
  toJSON = genericToJSON defaultOptions

-- Hash the snapshot
hashSnapshot :: Snapshot -> Either CanonicalError (ByteString, Text)
hashSnapshot snapshot = do
  canonicalBytes <- canonicalEncode (toJSON snapshot)
  let hash = sha256Hex canonicalBytes
  pure (canonicalBytes, hash)

-- Usage
main :: IO ()
main = do
  let snapshot = Snapshot
        { snapshotId = SnapshotId nil
        , facts =
          [ FactEntry "Battery" "SKU-123" "abc123..."
          , FactEntry "PCF" "SKU-123" "def456..."
          ]
        }

  case hashSnapshot snapshot of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (bytes, hash) -> do
      putStrLn $ "Canonical: " ++ show bytes
      putStrLn $ "Hash: " ++ T.unpack hash
```

## Testing

### Run All Tests

```bash
# Run full test suite
cabal test bpc-core

# Run with verbose output
cabal test bpc-core --test-show-details=always

# Run specific test module
cabal test bpc-core --test-option="--pattern=CanonicalJson"
```

### Run Property Tests

```bash
# Run only property tests
cabal test bpc-core --test-option="--pattern=prop_"

# Run with more samples
cabal test bpc-core --test-option="--quickcheck-tests=10000"
```

### Run Golden Tests

```bash
# Run golden tests (hash fixtures)
cabal test bpc-core --test-option="--pattern=golden"
```

### Check Test Coverage

```bash
# Generate coverage report
cabal test bpc-core --enable-coverage

# View report (location will be printed after tests)
# Typically: dist-newstyle/build/.../hpc/vanilla/html/bpc-core/hpc_index.html
```

## Common Patterns

### Pattern 1: Hashing JSON for Storage

```haskell
import BPC.Core.CanonicalJson
import BPC.Core.Hash

storeWithHash :: ToJSON a => a -> Either CanonicalError (ByteString, Text)
storeWithHash obj = do
  canonical <- canonicalEncode (toJSON obj)
  let hash = sha256Hex canonical
  pure (canonical, hash)
```

### Pattern 2: Building QR Payload Hash

```haskell
import BPC.Core.Hash

buildQrPayload :: PassportVersionId -> ByteString -> ByteString -> ByteString -> Text
buildQrPayload pvId payloadBytes proofBytes receiptBytes =
  let ph = base32NoPad (sha256Bytes payloadBytes)
      pr = base32NoPad (sha256Bytes proofBytes)
      rh = base32NoPad (sha256Bytes receiptBytes)
  in "BPC1|pv=" <> toText pvId <> "|ph=" <> ph <> "|pr=" <> pr <> "|rh=" <> rh
```

### Pattern 3: Type-Safe Repository Functions

```haskell
-- In bpc-db (not bpc-core)
import BPC.Core.Types.Domain

-- Type safety prevents argument swapping:
getPassport :: TenantId -> PassportId -> IO (Maybe Passport)
getPassport tenantId passportId = ...

-- These won't compile:
-- getPassport passportId tenantId  -- TYPE ERROR! Arguments swapped
-- getPassport tenantId documentId  -- TYPE ERROR! Wrong ID type
```

### Pattern 4: Unit-Safe Calculations

```haskell
{-# LANGUAGE DataKinds #-}

import BPC.Core.Types.Quantity
import BPC.Core.Types.Decimal

-- Calculate emission intensity: gCO2e / kWh
calculateIntensity :: Qty "gCO2e" -> Qty "kWh" -> Either QuantityError (Dec 6)
calculateIntensity emissions capacity = do
  -- Extract Dec 6 values
  let em = unQty emissions
  let cap = unQty capacity
  -- Division (check for zero)
  if cap == Dec 0
    then Left DivisionByZero
    else Right $ Dec ((unDec em * 1000000) `div` unDec cap)
```

## Verification Commands

### Verify SSOT Hash Fixture

```bash
# In GHCi:
$ cabal repl bpc-core

ghci> import BPC.Core.Hash
ghci> import Data.Text.Encoding (encodeUtf8)
ghci> sha256Hex (encodeUtf8 "{\"a\":1,\"b\":2}")
"43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777"
```

### Verify Canonical Encoding

```bash
ghci> import BPC.Core.CanonicalJson
ghci> import Data.Aeson (object, (.=))
ghci> canonicalEncode (object ["b" .= (2::Int), "a" .= (1::Int)])
Right "{\"a\":1,\"b\":2}"
```

### Verify Float Rejection

```bash
ghci> canonicalEncode (object ["x" .= (1.5::Double)])
Left (CanonicalNumberNotAllowed 1.5)
```

### Verify Type Safety

```bash
ghci> import BPC.Core.Types.Domain
ghci> import Data.UUID (nil)
ghci> let tid = TenantId nil
ghci> let pid = PassportId nil
ghci> :type tid
tid :: TenantId
ghci> let confusion :: TenantId = pid
-- TYPE ERROR: Couldn't match type 'PassportId' with 'TenantId'
```

## Troubleshooting

### Issue: "Could not find module BPC.Core"

**Solution**: Make sure you've built the package:
```bash
cabal build bpc-core
```

### Issue: Type-level errors with Dec or Qty

**Solution**: Make sure you enable necessary extensions:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
```

### Issue: "No instance for KnownNat..."

**Solution**: Use `TypeApplications` to specify the scale/unit:
```haskell
-- Instead of:
let d = Dec 100 :: Dec 6  -- ambiguous

-- Use:
let d = Dec @6 100  -- explicit type application
```

### Issue: Canonical encoding fails with "CanonicalNumberNotAllowed"

**Solution**: Your JSON contains floats. Convert to integers:
```haskell
-- Bad:
object ["capacity" .= (75.5 :: Double)]

-- Good (with Dec):
object ["capacity" .= toJSON (Dec @6 75500000)]  -- 75.5 with 6 decimals
```

## Next Steps

After mastering core primitives:

1. **03-rule-engine**: Use these types for DSL parsing and type-checking
2. **04-compilation-pipeline**: Use canonical encoding for proof/receipt generation
3. **05-data-layer**: Use domain IDs for all repository functions
4. **06-api-server**: Use canonical JSON for response serialization

## Resources

- **SSOT.md**: Sections 5.5, 7.1-7.2, 8.1, 8.3 for normative specifications
- **Haddock Documentation**: `cabal haddock bpc-core --open`
- **Test Suite**: `test/BPC/Core/` for more examples
- **Constitution.md**: Core principles (Determinism, Canonical Storage, Type Safety)
