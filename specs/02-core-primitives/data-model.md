# Data Model: Core Primitives

**Feature**: 02-core-primitives
**Date**: 2025-12-28
**SSOT Reference**: Sections 5.5, 7.1-7.2, 8.1, 8.3
**Package**: bpc-core

## Overview

Core primitives are pure Haskell types with no database representation. This document defines all type signatures, their semantics, and JSON serialization behavior.

## Module Structure

```haskell
BPC.Core
├── CanonicalJson    -- BPC-CJSON-1 encoding/decoding
├── Hash             -- SHA-256, Base32
├── Types
│   ├── Domain       -- All domain ID newtypes
│   ├── Decimal      -- Dec with type-level scale
│   ├── Quantity     -- Qty with type-level unit
│   └── Units        -- Unit definitions and conversions
└── Error            -- All error types
```

## 1. Canonical JSON (BPC.Core.CanonicalJson)

### Types

```haskell
module BPC.Core.CanonicalJson
  ( -- * Encoding/Decoding
    canonicalEncode
  , canonicalDecode
    -- * Errors
  , CanonicalError(..)
  ) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)

-- | Errors that can occur during canonical JSON encoding/decoding
data CanonicalError
  = CanonicalNumberNotAllowed Scientific
    -- ^ The JSON number is not a pure integer (has fractional part or exponent)
  | CanonicalDecodeFailed Text
    -- ^ Failed to decode canonical bytes as valid JSON
  deriving (Eq, Show)

-- | Encode an Aeson Value to canonical bytes
--
-- Properties:
-- - Keys sorted by UTF-8 byte order
-- - No whitespace
-- - Only integer numbers (rejects floats, exponents)
-- - Deterministic: identical inputs produce identical outputs
--
-- Example:
-- >>> canonicalEncode (object ["b" .= (2::Int), "a" .= (1::Int)])
-- Right "{\"a\":1,\"b\":2}"
canonicalEncode :: Value -> Either CanonicalError ByteString

-- | Decode canonical bytes back to an Aeson Value
--
-- Example:
-- >>> canonicalDecode "{\"a\":1,\"b\":2}"
-- Right (Object (fromList [("a",Number 1),("b",Number 2)]))
canonicalDecode :: ByteString -> Either CanonicalError Value
```

### JSON Examples

```json
// Input (any order, whitespace):
{
  "battery": {
    "capacity_kwh": 75
  },
  "manufacturer": "ACME"
}

// Canonical output (sorted keys, no whitespace):
{"battery":{"capacity_kwh":75},"manufacturer":"ACME"}
```

## 2. Cryptographic Hashing (BPC.Core.Hash)

### Types

```haskell
module BPC.Core.Hash
  ( -- * SHA-256
    sha256Hex
  , sha256Bytes
    -- * Base32
  , base32NoPad
  , base32Decode
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Compute SHA-256 hash and return as lowercase hex string
--
-- Output format: 64 characters [0-9a-f]
--
-- Example:
-- >>> sha256Hex "{\"a\":1,\"b\":2}"
-- "43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777"
sha256Hex :: ByteString -> Text

-- | Compute SHA-256 hash and return as raw 32 bytes
sha256Bytes :: ByteString -> ByteString

-- | Encode bytes to Base32 (RFC 4648) without padding
--
-- Character set: A-Z, 2-7 (uppercase)
-- No padding (no '=' characters)
--
-- Example:
-- >>> base32NoPad "\x00\x01\x02\x03"
-- "AAAQEAY"
base32NoPad :: ByteString -> Text

-- | Decode Base32 string to bytes
--
-- Accepts uppercase and lowercase, with or without padding
base32Decode :: Text -> Either Text ByteString
```

### Hash Fixtures (from SSOT 7.2)

| Input (canonical JSON) | SHA-256 Hash (hex) |
|------------------------|---------------------|
| `{"a":1,"b":2}` | `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777` |

## 3. Domain ID Types (BPC.Core.Types.Domain)

All domain IDs are UUID-based newtypes providing compile-time type safety.

### Type Definitions

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BPC.Core.Types.Domain
  ( -- * Tenant & Auth
    TenantId(..)
  , ActorId(..)
  , ApiKeyId(..)
  , RoleId(..)
    -- * Documents & Facts
  , DocumentId(..)
  , DocumentVersionId(..)
  , FactId(..)
    -- * Snapshots
  , SnapshotId(..)
  , SnapshotItemId(..)
    -- * Passports
  , BatteryProductId(..)
  , PassportId(..)
  , PassportVersionId(..)
    -- * Rules
  , RulePackageId(..)
  , RulePackageVersionId(..)
    -- * Jobs & Events
  , JobId(..)
  , EventId(..)
    -- * Policies
  , PolicyId(..)
  , PolicyVersionId(..)
    -- * Webhooks
  , WebhookEndpointId(..)
  , WebhookSubscriptionId(..)
  , WebhookDeliveryId(..)
  ) where

import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON)

-- Template for all ID types:
-- newtype XxxId = XxxId { unXxxId :: UUID }
--   deriving stock (Eq, Ord, Show)
--   deriving newtype (ToJSON, FromJSON)

-- | Tenant identifier (multi-tenancy isolation)
newtype TenantId = TenantId { unTenantId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Actor identifier (user, API client, or service)
newtype ActorId = ActorId { unActorId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | API Key identifier
newtype ApiKeyId = ApiKeyId { unApiKeyId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Role identifier (RBAC)
newtype RoleId = RoleId { unRoleId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Document container identifier
newtype DocumentId = DocumentId { unDocumentId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | DocumentVersion identifier (immutable upload)
newtype DocumentVersionId = DocumentVersionId { unDocumentVersionId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Fact identifier (structured data from documents)
newtype FactId = FactId { unFactId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | DataSnapshot identifier
newtype SnapshotId = SnapshotId { unSnapshotId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | SnapshotItem identifier
newtype SnapshotItemId = SnapshotItemId { unSnapshotItemId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | BatteryProduct identifier
newtype BatteryProductId = BatteryProductId { unBatteryProductId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Passport container identifier
newtype PassportId = PassportId { unPassportId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | PassportVersion identifier (immutable compilation result)
newtype PassportVersionId = PassportVersionId { unPassportVersionId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | RulePackage container identifier
newtype RulePackageId = RulePackageId { unRulePackageId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | RulePackageVersion identifier (immutable DSL source)
newtype RulePackageVersionId = RulePackageVersionId { unRulePackageVersionId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Job identifier (async task)
newtype JobId = JobId { unJobId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Event identifier (audit log entry)
newtype EventId = EventId { unEventId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Policy container identifier
newtype PolicyId = PolicyId { unPolicyId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | PolicyVersion identifier (immutable access rule)
newtype PolicyVersionId = PolicyVersionId { unPolicyVersionId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | WebhookEndpoint identifier
newtype WebhookEndpointId = WebhookEndpointId { unWebhookEndpointId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | WebhookSubscription identifier
newtype WebhookSubscriptionId = WebhookSubscriptionId { unWebhookSubscriptionId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

-- | WebhookDelivery identifier
newtype WebhookDeliveryId = WebhookDeliveryId { unWebhookDeliveryId :: UUID }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)
```

### Type Safety Properties

```haskell
-- Compiler prevents type confusion:
let tid :: TenantId = ...
let pid :: PassportId = ...

-- This will NOT compile:
let confusion :: TenantId = pid  -- TYPE ERROR!

-- Must explicitly convert:
let uuid = unPassportId pid
let wrapped = TenantId uuid  -- OK (if you really mean it)
```

## 4. Decimal Arithmetic (BPC.Core.Types.Decimal)

Fixed-precision decimal with type-level scale enforcement.

### Types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module BPC.Core.Types.Decimal
  ( Dec(..)
  , decScale
  , rescale
  , parseDec
  , renderDec
  , DecimalError(..)
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Data.Text (Text)

-- | Fixed-point decimal with type-level scale
--
-- The scale is a type-level natural number indicating decimal places.
-- Internally represented as Integer scaled by 10^scale.
--
-- Examples:
-- - Dec 2 represents 2 decimal places: Dec 2 100 = 1.00
-- - Dec 6 represents 6 decimal places: Dec 6 1000000 = 1.000000
newtype Dec (scale :: Nat) = Dec { unDec :: Integer }
  deriving stock (Eq, Ord, Show)

-- | Get the scale of a Dec value at runtime
--
-- Example:
-- >>> decScale (Dec @6 1000000)
-- 6
decScale :: forall s. KnownNat s => Dec s -> Integer
decScale _ = natVal (Proxy @s)

-- | Scaling factor: 10^scale
scaleFactor :: forall s. KnownNat s => Proxy s -> Integer
scaleFactor _ = 10 ^ natVal (Proxy @s)

-- | Convert between different scales
--
-- Example:
-- >>> rescale @2 @6 (Dec 100)  -- 1.00 -> 1.000000
-- Dec 1000000
rescale :: forall s t. (KnownNat s, KnownNat t) => Dec s -> Dec t
rescale (Dec n) =
  let sf = scaleFactor (Proxy @s)
      tf = scaleFactor (Proxy @t)
  in Dec ((n * tf) `div` sf)

-- | Arithmetic instances (same scale only for +/-)
instance KnownNat s => Num (Dec s) where
  Dec a + Dec b = Dec (a + b)
  Dec a - Dec b = Dec (a - b)
  Dec a * Dec b = Dec ((a * b) `div` scaleFactor (Proxy @s))
  abs (Dec a) = Dec (abs a)
  signum (Dec a) = Dec (signum a * scaleFactor (Proxy @s))
  fromInteger n = Dec (n * scaleFactor (Proxy @s))

-- | Parse decimal from text representation
--
-- Examples:
-- >>> parseDec @2 "1.50"
-- Right (Dec 150)
-- >>> parseDec @6 "0.000001"
-- Right (Dec 1)
parseDec :: forall s. KnownNat s => Text -> Either DecimalError (Dec s)

-- | Render decimal to text representation
--
-- Example:
-- >>> renderDec (Dec @2 150)
-- "1.50"
renderDec :: forall s. KnownNat s => Dec s -> Text

-- | Decimal parsing/rendering errors
data DecimalError
  = DecimalParseError Text
  | DecimalScaleMismatch { expected :: Int, actual :: Int }
  deriving (Eq, Show)

-- | JSON serialization (as string to preserve precision)
instance KnownNat s => ToJSON (Dec s) where
  toJSON = String . renderDec

instance KnownNat s => FromJSON (Dec s) where
  parseJSON = withText "Dec" $ \t ->
    case parseDec t of
      Left err -> fail (show err)
      Right d -> pure d
```

### Arithmetic Rules

| Operation | Type Constraint | Result |
|-----------|----------------|--------|
| `Dec s + Dec s` | Same scale | `Dec s` |
| `Dec s - Dec s` | Same scale | `Dec s` |
| `Dec s * Dec s` | Same scale | `Dec s` |
| `Dec s / Dec s` | Same scale | `Either DivByZero (Dec s)` |
| `rescale @s @t` | Any scales | `Dec t` |

### Examples

```haskell
-- Creating Dec values:
let price :: Dec 2 = Dec 1299  -- $12.99
let tax :: Dec 2 = Dec 78      -- $0.78
let total = price + tax         -- Dec 2 (Dec 1377) = $13.77

-- Scale conversion:
let highPrecision = rescale @2 @6 price  -- Dec 6 (Dec 1299000000)

-- Frominteger:
let five :: Dec 6 = 5  -- Dec 5000000 (5.000000)
```

## 5. Quantity with Units (BPC.Core.Types.Quantity)

Physical quantities with type-level unit enforcement.

### Types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module BPC.Core.Types.Quantity
  ( Qty(..)
  , addQty
  , subQty
  , mulQtyDec
  , divQtyDec
  , QuantityError(..)
  ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import BPC.Core.Types.Decimal (Dec)

-- | Quantity with type-level unit
--
-- The unit is a type-level symbol (string).
-- Internally uses Dec 6 for all quantities (6 decimal places).
--
-- Examples:
-- - Qty "kg" (Dec 450250000) = 450.25 kg
-- - Qty "kWh" (Dec 75500000) = 75.5 kWh
data Qty (unit :: Symbol) = Qty { unQty :: Dec 6 }
  deriving stock (Eq, Ord, Show)

-- | Add two quantities (same unit enforced by types)
addQty :: Qty u -> Qty u -> Qty u
addQty (Qty a) (Qty b) = Qty (a + b)

-- | Subtract two quantities
subQty :: Qty u -> Qty u -> Qty u
subQty (Qty a) (Qty b) = Qty (a - b)

-- | Multiply quantity by decimal scalar
mulQtyDec :: KnownNat s => Qty u -> Dec s -> Qty u
mulQtyDec (Qty q) d = Qty (q * rescale @s @6 d)

-- | Divide quantity by decimal scalar
divQtyDec :: KnownNat s => Qty u -> Dec s -> Either QuantityError (Qty u)
divQtyDec (Qty q) d
  | unDec (rescale @s @6 d) == 0 = Left DivisionByZero
  | otherwise = Right $ Qty (q / rescale @s @6 d)

-- | Quantity arithmetic errors
data QuantityError
  = DivisionByZero
  | UnitMismatch Text Text  -- expected, actual
  deriving (Eq, Show)

-- | JSON serialization (as object with value and unit)
instance KnownSymbol u => ToJSON (Qty u) where
  toJSON (Qty d) = object
    [ "value" .= renderDec d
    , "unit" .= symbolVal (Proxy @u)
    ]

instance KnownSymbol u => FromJSON (Qty u) where
  parseJSON = withObject "Qty" $ \o -> do
    value <- o .: "value" >>= parseJSON
    unit <- o .: "unit"
    unless (unit == symbolVal (Proxy @u)) $
      fail $ "Unit mismatch: expected " <> symbolVal (Proxy @u) <> ", got " <> unit
    pure (Qty value)
```

### Arithmetic Rules (SSOT 8.3)

| Operation | Type Constraint | Result | Notes |
|-----------|----------------|--------|-------|
| `Qty u + Qty u` | Same unit | `Qty u` | Type enforced |
| `Qty u - Qty u` | Same unit | `Qty u` | Type enforced |
| `Qty u * Dec s` | Any scale | `Qty u` | Scalar multiplication |
| `Qty u / Dec s` | Any scale | `Either QuantityError (Qty u)` | Division by zero check |
| `Qty u1 + Qty u2` | Different units | **TYPE ERROR** | Compiler prevents |

### Examples

```haskell
-- Creating quantities:
let mass :: Qty "kg" = Qty (Dec 450250000)       -- 450.25 kg
let energy :: Qty "kWh" = Qty (Dec 75500000)     -- 75.5 kWh

-- Same-unit operations:
let totalMass = addQty mass mass                  -- Qty "kg" (900.5 kg)

-- These will NOT compile:
-- let invalid = addQty mass energy               -- TYPE ERROR!
-- let wrong :: Qty "g" = mass                    -- TYPE ERROR!

-- Scalar operations:
let doubled = mulQtyDec mass (Dec @6 2000000)     -- Qty "kg" (900.5 kg)
```

## 6. Unit Definitions (BPC.Core.Types.Units)

Runtime unit representation and conversions.

### Types

```haskell
module BPC.Core.Types.Units
  ( Unit(..)
  , parseUnit
  , renderUnit
  , conversionFactor
  , convert
  , UnitError(..)
  ) where

import BPC.Core.Types.Decimal (Dec)

-- | Supported physical units (SSOT 8.1)
data Unit
  = Kg          -- ^ Kilogram (mass base unit)
  | G           -- ^ Gram (mass)
  | KWh         -- ^ Kilowatt-hour (energy base unit)
  | Wh          -- ^ Watt-hour (energy)
  | GCO2e       -- ^ Gram CO2 equivalent (emissions)
  | KgCO2e      -- ^ Kilogram CO2 equivalent (emissions base unit)
  | GCO2ePerKWh -- ^ Emission intensity (not convertible)
  | Pct         -- ^ Percentage 0-100 (dimensionless, not convertible)
  | Each        -- ^ Count (dimensionless, not convertible)
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Parse unit from text
parseUnit :: Text -> Maybe Unit
parseUnit = \case
  "kg"            -> Just Kg
  "g"             -> Just G
  "kWh"           -> Just KWh
  "Wh"            -> Just Wh
  "gCO2e"         -> Just GCO2e
  "kgCO2e"        -> Just KgCO2e
  "gCO2e_per_kWh" -> Just GCO2ePerKWh
  "pct"           -> Just Pct
  "each"          -> Just Each
  _               -> Nothing

-- | Render unit to text
renderUnit :: Unit -> Text
renderUnit = \case
  Kg            -> "kg"
  G             -> "g"
  KWh           -> "kWh"
  Wh            -> "Wh"
  GCO2e         -> "gCO2e"
  KgCO2e        -> "kgCO2e"
  GCO2ePerKWh   -> "gCO2e_per_kWh"
  Pct           -> "pct"
  Each          -> "each"

-- | Conversion factor from one unit to another
--
-- Returns factor such that: fromValue * factor = toValue
conversionFactor :: Unit -> Unit -> Either UnitError (Dec 6)
conversionFactor from to = case (from, to) of
  -- Same unit
  (u, v) | u == v -> Right (Dec 1000000)  -- 1.0

  -- Mass conversions
  (G, Kg)   -> Right (Dec 1000)           -- 0.001
  (Kg, G)   -> Right (Dec 1000000000)     -- 1000.0

  -- Energy conversions
  (Wh, KWh) -> Right (Dec 1000)           -- 0.001
  (KWh, Wh) -> Right (Dec 1000000000)     -- 1000.0

  -- Emission conversions
  (GCO2e, KgCO2e)  -> Right (Dec 1000)    -- 0.001
  (KgCO2e, GCO2e)  -> Right (Dec 1000000000)  -- 1000.0

  -- Incompatible units
  _ -> Left (ConversionNotSupported from to)

-- | Convert value from one unit to another
convert :: Unit -> Unit -> Dec 6 -> Either UnitError (Dec 6)
convert from to val = do
  factor <- conversionFactor from to
  pure $ Dec ((unDec val * unDec factor) `div` 1000000)

-- | Unit conversion errors
data UnitError
  = ConversionNotSupported Unit Unit
  | UnitUnknown Text
  deriving (Eq, Show)
```

### Conversion Table

| From | To | Factor | Example |
|------|-----|--------|---------|
| g | kg | 0.001 | 1000 g = 1 kg |
| kg | g | 1000 | 1 kg = 1000 g |
| Wh | kWh | 0.001 | 1000 Wh = 1 kWh |
| kWh | Wh | 1000 | 1 kWh = 1000 Wh |
| gCO2e | kgCO2e | 0.001 | 1000 gCO2e = 1 kgCO2e |
| kgCO2e | gCO2e | 1000 | 1 kgCO2e = 1000 gCO2e |
| kg | kWh | ERROR | Incompatible |
| pct | kg | ERROR | Dimensionless |

## 7. Error Types (BPC.Core.Error)

Consolidated error types for all core primitives.

```haskell
module BPC.Core.Error
  ( -- * Re-exports
    module BPC.Core.CanonicalJson
  , module BPC.Core.Types.Decimal
  , module BPC.Core.Types.Quantity
  , module BPC.Core.Types.Units
  ) where

-- All error types re-exported for convenience:
-- - CanonicalError
-- - DecimalError
-- - QuantityError
-- - UnitError
```

## Summary

| Type | Purpose | Key Properties |
|------|---------|----------------|
| `Value` | Aeson JSON value | Standard Aeson type |
| `CanonicalError` | Canonical encoding errors | Float rejection, decode failures |
| `TenantId`, etc. | Domain IDs | Type-safe UUID wrappers, zero runtime cost |
| `Dec (scale :: Nat)` | Fixed-precision decimal | Type-level scale, unbounded Integer backend |
| `Qty (unit :: Symbol)` | Physical quantity | Type-level unit, Dec 6 backend |
| `Unit` | Runtime unit enum | 9 units, conversion table |

All types are pure (no IO), deterministic, and type-safe.
