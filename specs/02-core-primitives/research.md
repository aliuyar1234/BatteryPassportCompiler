# Research: Core Primitives

**Feature**: 02-core-primitives
**Date**: 2025-12-28
**Status**: Complete

## Overview

This document captures the research and design decisions for BatteryPassportCompiler's core primitives: canonical JSON encoding, cryptographic hashing, Base32 encoding, domain ID types, and quantity arithmetic with units.

## 1. Canonical JSON Encoding (BPC-CJSON-1)

### Problem Statement

BatteryPassportCompiler requires byte-identical determinism: identical inputs must produce byte-identical outputs. Standard JSON serialization is non-deterministic because:
1. Object key order is unspecified
2. Whitespace is arbitrary
3. Number representations vary (exponents, trailing zeros)

### Specification: BPC-CJSON-1

**Requirements** (from SSOT 7.1):
- Object keys sorted by UTF-8 byte order
- No whitespace
- Only integer numbers allowed (no floats, no exponents)
- Escape sequences normalized

**Algorithm**:
```text
canonicalEncode(v):
  Null → "null"
  Bool → "true" | "false"
  String → JSON-escaped with normalized escapes
  Number(n):
    IF n has exponent OR fractional part THEN
      FAIL CanonicalNumberNotAllowed
    ELSE
      encode as integer
  Array → "[" + join(",", map(canonicalEncode, items)) + "]"
  Object → "{" + join(",", for k in sortUtf8(keys):
            escape(k) + ":" + canonicalEncode(v[k])) + "}"
```

**Example**:
- Input: `{"b": 2, "a": 1}` (pretty-printed, arbitrary key order)
- Output: `{"a":1,"b":2}` (canonical, no whitespace, sorted keys)

### Design Decisions

#### Decision 1: UTF-8 Byte Order for Key Sorting

**Options Considered**:
1. Lexicographic string order (Unicode codepoints)
2. UTF-8 byte order
3. ASCII order (fail on non-ASCII)

**Chosen**: UTF-8 byte order

**Rationale**:
- UTF-8 byte order is unambiguous and reproducible across platforms
- Supports international characters (battery manufacturers worldwide)
- Matches common crypto implementations (Bitcoin, Ethereum use UTF-8 byte order)
- Haskell `Data.Text.Encoding.encodeUtf8` provides canonical UTF-8 encoding

**Trade-offs**:
- More complex than ASCII-only, but necessary for global deployment
- Sorting by bytes vs codepoints differs for multi-byte characters (acceptable, as consistency matters more than intuitive ordering)

#### Decision 2: Rejecting Floats and Exponents

**Options Considered**:
1. Allow floats, normalize to fixed precision
2. Allow floats, store exact representation
3. Reject floats entirely

**Chosen**: Reject floats entirely

**Rationale**:
- Floating-point has platform-dependent rounding (IEEE-754 edge cases)
- Different JSON libraries serialize floats differently (`1.0` vs `1` vs `1.00`)
- Battery passport data is inherently decimal (weights, capacities, percentages)
- Rule DSL uses `Dec(scale)` for fixed-precision arithmetic
- Eliminates entire class of determinism bugs

**Trade-offs**:
- Clients must convert floats to integers (e.g., `capacity_kwh: 75.5` → `75500` milliwatt-hours)
- Explicit scale management required (handled by `Dec` type)
- Acceptable: precision is critical for audit compliance

#### Decision 3: Aeson Integration

**Implementation**: Build on Aeson's `Value` type, override `encode` behavior

**Rationale**:
- Aeson is the de facto Haskell JSON library
- `Value` is already used throughout the codebase (SSOT 5.3)
- We only override serialization, not parsing
- Enables gradual migration (parse with Aeson, serialize with canonical encoder)

**Implementation Strategy**:
```haskell
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalEncode = go
  where
    go (Object o) =
      let sorted = sortBy (compare `on` (encodeUtf8 . fst)) (toList o)
      in ...
    go (Number n) =
      case toBoundedInteger n of
        Just i -> encode i
        Nothing -> Left CanonicalNumberNotAllowed
```

## 2. Cryptographic Hashing (BPC-HASH-1)

### Problem Statement

Need cryptographically secure hashing for:
- Event Store hash chains (tamper detection)
- Snapshot integrity verification
- Proof node hashing
- Receipt signing

### Specification: BPC-HASH-1

**Algorithm**: SHA-256
**Output Format**: 64-character lowercase hexadecimal string
**Example** (from SSOT 7.2):
- Input: `{"a":1,"b":2}` (canonical bytes)
- Output: `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777`

### Design Decisions

#### Decision 4: SHA-256 (not SHA-3, BLAKE2, etc.)

**Options Considered**:
1. SHA-256
2. SHA-3 (Keccak)
3. BLAKE2b
4. BLAKE3

**Chosen**: SHA-256

**Rationale**:
- Industry standard for blockchain/crypto (Bitcoin, Ethereum)
- Widely audited, no known practical attacks
- Hardware acceleration on modern CPUs (Intel SHA extensions)
- Cryptonite library provides high-quality Haskell bindings
- Regulatory familiarity (auditors understand SHA-256)

**Trade-offs**:
- Slightly slower than BLAKE2/BLAKE3 (acceptable: hashing is not the bottleneck)
- Longer output than BLAKE2s (32 bytes is fine for our use case)
- SHA-3 is newer but less mature ecosystem

**Performance**: 200+ MB/sec on typical server hardware (sufficient for passport compilation)

#### Decision 5: Lowercase Hex Encoding

**Options Considered**:
1. Lowercase hex
2. Uppercase hex
3. Base64
4. Base32

**Chosen**: Lowercase hex

**Rationale**:
- Human-readable and debuggable
- Copy-paste friendly (no case sensitivity issues)
- Matches Bitcoin/Ethereum conventions
- Fixed-width (64 characters) simplifies validation
- No URL encoding needed (unlike Base64 with `+/=`)

**Trade-offs**:
- 2x longer than raw bytes (acceptable: storage is cheap, readability is valuable)
- Slightly longer than Base64 (64 chars vs 43) but more readable

## 3. Base32 Encoding for QR Codes (BPC-QR-1)

### Problem Statement

QR codes have size limits; need compact encoding for hashes in QR payloads.

### Specification: BPC-QR-1

**Algorithm**: RFC 4648 Base32, no padding
**Character Set**: `ABCDEFGHIJKLMNOPQRSTUVWXYZ234567`
**Example**:
- Input: 32-byte hash
- Output: 52-character Base32 string (no `=` padding)

### Design Decisions

#### Decision 6: Base32 (not Base64)

**Options Considered**:
1. Base64
2. Base32
3. Hex

**Chosen**: Base32 without padding

**Rationale**:
- Case-insensitive (QR readers sometimes mangle case)
- No punctuation (avoids QR encoding mode switches)
- Shorter than hex (52 chars vs 64 for 32-byte hash)
- URL-safe without escaping
- Alphanumeric QR mode is more efficient than Binary for Base32

**Trade-offs**:
- 20% longer than Base64 (acceptable: QR can hold ~4000 chars in Alphanumeric mode)
- Less common than Base64 (acceptable: we control both encoding and decoding)

#### Decision 7: No Padding

**Rationale**:
- Padding (`=`) wastes space in QR codes
- Decoder can infer padding from string length
- Matches common crypto practices (Bitcoin addresses have no padding)

**Implementation**: Use `memory` library's `convertToBase Base32` and strip trailing `=`

## 4. Domain ID Types (Type Safety)

### Problem Statement

UUIDs are opaque; accidentally swapping TenantId and PassportId causes silent bugs.

### Specification

**Approach**: Newtype wrappers for all domain IDs (SSOT 5.5, 6.2)

**Example**:
```haskell
newtype TenantId = TenantId UUID
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype PassportId = PassportId UUID
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- Compiler prevents:
-- let tid :: TenantId = PassportId someUuid  -- TYPE ERROR
```

### Design Decisions

#### Decision 8: Newtype (not Type Alias)

**Options Considered**:
1. Type alias: `type TenantId = UUID`
2. Newtype: `newtype TenantId = TenantId UUID`
3. Data type: `data TenantId = TenantId UUID`

**Chosen**: Newtype

**Rationale**:
- Zero runtime cost (GHC optimizes away newtype wrappers)
- Compiler enforces type distinction (prevents TenantId/PassportId confusion)
- Deriving strategies work well (`deriving newtype (ToJSON, FromJSON)`)
- Haddock generates separate documentation per type

**Trade-offs**:
- Slightly more verbose than type alias (acceptable: explicitness is valuable)
- Cannot use UUID functions directly (must unwrap first; this is a feature, not a bug)

#### Decision 9: Deriving Strategy

**Chosen**: `deriving newtype` for instances that delegate to UUID

**Rationale**:
- `ToJSON`/`FromJSON`: UUIDs serialize as strings; newtype should behave identically
- `Eq`/`Ord`: Structural equality is correct
- `Show`: Default instance is readable for debugging

**Example**:
```haskell
newtype TenantId = TenantId UUID
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, FromHttpApiData)
```

## 5. Decimal Arithmetic (Fixed Precision)

### Problem Statement

Floats are non-deterministic (IEEE-754 rounding, platform differences). Need exact decimal arithmetic for battery passport data (capacity: 75.5 kWh, mass: 450.25 kg).

### Specification

**Approach**: Type-level scale with Integer backend

**Type**: `Dec (scale :: Nat)`
**Backend**: `Integer` (unbounded, exact)
**Scale**: Type-level natural number (0-18)

**Example**:
```haskell
-- 75.50 kWh represented as Dec 2 with value 7550
let capacity :: Dec 2 = Dec 7550

-- Type-level enforcement:
let a :: Dec 2 = Dec 100  -- 1.00
let b :: Dec 2 = Dec 250  -- 2.50
let c = a + b              -- Dec 2 (3.50), typechecks

let d :: Dec 6 = Dec 100000  -- 0.100000
let e = a + d                 -- TYPE ERROR: Dec 2 != Dec 6
```

### Design Decisions

#### Decision 10: Type-Level Scale (not Runtime)

**Options Considered**:
1. Runtime scale: `data Dec = Dec Integer Int` (value, scale)
2. Type-level scale: `newtype Dec (scale :: Nat) = Dec Integer`

**Chosen**: Type-level scale with DataKinds

**Rationale**:
- Compiler enforces scale compatibility (cannot add Dec 2 and Dec 6)
- Zero runtime overhead (scale is erased at runtime)
- Matches Rule DSL semantics (SSOT 8.1: `Dec(6)` is a type, not a value)
- Type errors are caught at compile time, not runtime

**Trade-offs**:
- More complex type signatures (acceptable: type errors prevent bugs)
- Cannot change scale dynamically (feature, not bug: explicit conversions required)

#### Decision 11: Integer Backend (not Rational, not Fixed)

**Options Considered**:
1. `Integer` (unbounded, scaled by 10^scale)
2. `Rational` (numerator/denominator)
3. `Fixed` from `Data.Fixed` library

**Chosen**: Integer with scaling factor

**Rationale**:
- Exact arithmetic (no rounding errors)
- Unbounded (no overflow for passport-scale numbers)
- Simple implementation (just scaled integer math)
- Deterministic across platforms

**Trade-offs**:
- Division requires rounding mode (handled explicitly in Rule DSL)
- Multiplication can overflow scale (mitigated: Rule DSL uses Qty for units)

## 6. Quantity Arithmetic (Physical Units)

### Problem Statement

Adding kg to kWh is nonsensical; need compile-time unit checking.

### Specification

**Approach**: Type-level unit with Dec 6 backend

**Type**: `Qty (unit :: Symbol)`
**Backend**: `Dec 6` (fixed 6 decimal places)
**Supported Units** (SSOT 8.1):
- Mass: kg, g
- Energy: kWh, Wh
- Emissions: gCO2e, kgCO2e, gCO2e_per_kWh
- Other: pct, each

**Example**:
```haskell
let mass :: Qty "kg" = Qty (Dec 450250000)  -- 450.25 kg
let energy :: Qty "kWh" = Qty (Dec 75500000)  -- 75.5 kWh

-- Type-level enforcement:
let total = mass + mass        -- Qty "kg", typechecks
let invalid = mass + energy    -- TYPE ERROR: "kg" != "kWh"

-- Scalar multiplication:
let doubled = mass * Dec 2     -- Qty "kg", typechecks
```

### Design Decisions

#### Decision 12: Type-Level Unit (not Runtime)

**Options Considered**:
1. Runtime unit: `data Qty = Qty Dec Text` (value, unit)
2. Type-level unit: `newtype Qty (unit :: Symbol) = Qty (Dec 6)`

**Chosen**: Type-level unit with DataKinds

**Rationale**:
- Compiler enforces unit compatibility (cannot add kg and kWh)
- Matches Rule DSL semantics (SSOT 8.3: `Qty("kg")` is a type)
- Zero runtime overhead (unit is erased)
- Type errors > runtime errors

**Trade-offs**:
- Conversion requires explicit function call (feature: forces developer to think about units)
- Cannot parse unit from string at runtime (acceptable: units are known at compile time in Rule DSL)

#### Decision 13: Fixed Backend Dec 6

**Rationale**:
- All physical quantities use 6 decimal places in passport data
- Consistent precision for all units
- Simplifies arithmetic (no scale conversions within Qty)

**Trade-offs**:
- Cannot use Dec 2 for percentages (acceptable: convert at boundary)
- 6 decimals might be overkill for some units (acceptable: precision is cheap)

#### Decision 14: Conversion Table Design

**Approach**: Hardcoded conversion factors in `Units.hs`

**Example**:
```haskell
data UnitDef = UnitDef
  { unitSymbol :: Text
  , unitName :: Text
  , baseUnit :: Maybe Text      -- Nothing if base unit
  , conversionFactor :: Maybe Rational  -- conversion to base
  }

-- Example:
kg_unit = UnitDef "kg" "kilogram" Nothing Nothing
g_unit = UnitDef "g" "gram" (Just "kg") (Just (1 % 1000))

-- Conversion:
convert "g" (Qty 1000) = Qty 1 :: Qty "kg"
```

**Rationale**:
- Simple, explicit conversion table
- Type-safe: conversions verified at compile time where possible
- Easily auditable (table is human-readable)
- Supports only necessary conversions (no kg → kWh)

**Trade-offs**:
- Must manually add new units (acceptable: unit list is stable)
- No dimensional analysis (acceptable: our units are simple)

## 7. Error Handling Strategy

### Design Decisions

#### Decision 15: Either vs Exception

**Chosen**: `Either` for pure functions, custom error types

**Rationale**:
- Matches Haskell best practices (explicit errors in types)
- Forces callers to handle errors (no silent exceptions)
- Enables error aggregation in Rule DSL evaluation
- Maintains purity (IO-free requirement)

**Error Types**:
```haskell
data CanonicalError
  = CanonicalNumberNotAllowed
  | CanonicalDecodeFailed Text
  deriving (Eq, Show)

data UnitError
  = UnitMismatch Text Text  -- expected, actual
  | UnitUnknown Text
  | ConversionNotSupported Text Text
  deriving (Eq, Show)
```

## Alternatives Considered and Rejected

### 1. JSON-LD for Canonical Encoding
**Rejected**: Too complex, introduces semantic dependencies, not needed for determinism

### 2. CBOR Instead of JSON
**Rejected**: Battery passport ecosystem expects JSON (regulatory, interop)

### 3. Runtime Unit System (like F# Units of Measure)
**Rejected**: Haskell's type-level programming is sufficient, runtime adds overhead

### 4. Decimal Library (Data.Decimal)
**Rejected**: Runtime scale is less safe than type-level scale

### 5. Floating-Point with Epsilon Comparison
**Rejected**: Violates determinism requirement, introduces subtle bugs

## Open Questions

### Q1: Should we support custom units via configuration?
**Decision**: No, hardcode the 9 units from SSOT 8.1
**Rationale**: Unit list is stable, type-level enforcement requires compile-time knowledge

### Q2: How to handle division rounding?
**Decision**: Defer to Rule DSL evaluation (bpc-core primitives use truncation, Rule DSL can specify rounding mode)

### Q3: Should Dec support negative scales (e.g., Dec -2 for hundreds)?
**Decision**: No, limit to scales 0-18 (covers all passport use cases)

## References

- **SSOT 7.1**: Canonical JSON specification
- **SSOT 7.2**: SHA-256 specification and fixtures
- **SSOT 7.8**: Base32 QR encoding
- **SSOT 8.1**: Supported units
- **SSOT 8.3**: Type-checking rules for quantities
- **Constitution I**: Determinism principle
- **Constitution II**: Canonical storage principle
- **Constitution VI**: Type-safe rules principle

## Revision History

| Date | Author | Changes |
|------|--------|---------|
| 2025-12-28 | Initial | Complete research document for core primitives |
