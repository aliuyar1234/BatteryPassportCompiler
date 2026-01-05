# Quickstart: Rule Engine (DSL)

**Feature**: 03-rule-engine
**Date**: 2025-12-28
**Package**: bpc-core

## Prerequisites

- 01-foundation and 02-core-primitives complete
- GHC 9.6.4 with GADTs/DataKinds support

## DSL Syntax Overview

### Field Declarations

```text
field <path>: <type> = <expression>;

-- Example
field battery.capacity_kwh: Dec(6) =
  let b = getFact("Battery", "battery:SKU-123");
  let raw = requireSome(b, "E001", "battery missing");
  toDec(6, recordGet(raw, "capacity_kwh"));
```

### Supported Types

| Type | Syntax | Description |
|------|--------|-------------|
| Boolean | `Bool` | true/false |
| Integer | `Int` | Unbounded integer |
| Decimal | `Dec(n)` | Fixed-point with n decimal places |
| Quantity | `Qty(unit)` | Numeric with physical unit |
| Text | `Text` | Unicode string |
| Date | `Date` | Calendar date |
| Optional | `Opt(T)` | Some(value) or none |
| List | `List(T)` | Homogeneous list |
| Record | `Record(...)` | Named fields |

### Operators

| Operator | Types | Description |
|----------|-------|-------------|
| `+`, `-` | Same type | Add/subtract |
| `*` | Qty × Dec | Multiply quantity |
| `/` | Qty ÷ Dec | Divide quantity |
| `==`, `!=` | Any comparable | Equality |
| `<`, `<=`, `>`, `>=` | Ordered | Comparison |
| `&&`, `\|\|` | Bool | Logical |
| `!` | Bool | Negation |

### Built-in Functions

```text
-- Fact access
getFact(type, key) -> Opt(Record)
getFactsByPrefix(type, prefix) -> List(Record)

-- Field reference
field(path) -> any

-- Record operations
recordGet(record, field) -> Opt(any)

-- Option handling
isSome(opt) -> Bool
unwrapOr(opt, default) -> value
requireSome(opt, errorCode, message) -> value

-- Type conversion
toDec(scale, value) -> Dec(scale)
toQty(unit, value) -> Qty(unit)
convert(fromUnit, toUnit, qty) -> Qty

-- Aggregation
sumQty(unit, list) -> Qty
sumDec(scale, list) -> Dec

-- List operations
map(list, fn) -> List
filter(list, predicate) -> List
fold(list, initial, fn) -> value

-- Compliance
emitCompliance(clause, status, evidence) -> Bool
```

## Quick Usage

### Parse DSL Source

```haskell
import BPC.Core.Rules.Parser
import qualified Data.Text.IO as T

main :: IO ()
main = do
  source <- T.readFile "rules.dsl"
  case parseSource source of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right decls -> putStrLn $ "Parsed " ++ show (length decls) ++ " declarations"
```

### Typecheck Rules

```haskell
import BPC.Core.Rules.Parser
import BPC.Core.Rules.Typecheck

main :: IO ()
main = do
  source <- T.readFile "rules.dsl"
  case parseSource source of
    Left err -> error $ show err
    Right decls -> case typecheckDecls decls of
      Left tcErr -> putStrLn $ "Type error: " ++ show tcErr
      Right typed -> putStrLn "Typecheck passed!"
```

### Build Dependency Graph

```haskell
import BPC.Core.Rules.Graph

-- Get sorted field order
case topoSortFields fieldMap of
  Left (CycleError fields) ->
    error $ "Cycle detected: " ++ show fields
  Right sorted ->
    forM_ sorted $ \path ->
      putStrLn $ "Evaluating: " ++ show path
```

### Evaluate Rules

```haskell
import BPC.Core.Rules.Eval

-- Build evaluation context
let ctx = EvalContext
  { facts = Map.fromList
      [ (("Battery", "battery:SKU-123"), batteryRecord)
      , (("PCF", "pcf:SKU-123"), pcfRecord)
      ]
  , fields = Map.empty  -- Will be filled during eval
  }

-- Evaluate all fields
case evalFields typedExprs ctx of
  Left err -> error $ show err
  Right results -> print results
```

## Example DSL

### Minimal Example (SSOT 8.4)

```text
-- Example rules for a minimal passport

field battery.capacity_kwh: Dec(6) =
  let b = getFact("Battery", "battery:SKU-123");
  let cap = requireSome(
    recordGet(requireSome(b, "E001", "battery missing"), "capacity_kwh"),
    "E002", "capacity missing"
  );
  toDec(6, cap);

field sustainability.pcf_gco2e_per_kwh: Dec(2) =
  let p = getFact("PCF", "pcf:SKU-123");
  let total = requireSome(
    recordGet(requireSome(p, "E010", "pcf missing"), "total_gco2e"),
    "E011", "missing total"
  );
  let cap = field("battery.capacity_kwh");
  toDec(2, toDec(2, total) / cap);
```

### Example Test

```text
example test_battery_capacity: {
  fact battery("Battery", "battery:SKU-123") = {
    "capacity_kwh": 50000000
  };
} => expect(battery.capacity_kwh, ==, 50.000000);
```

### Property Test

```text
property positive_capacity: cases(500) seed(42) =>
  forall cap: Dec(6).
    implies(cap > 0, field("battery.capacity_kwh") > 0);
```

## Testing

### Run Parser Tests

```bash
cabal test bpc-core --test-option="--pattern=Parser"
```

### Run Type Checker Tests

```bash
cabal test bpc-core --test-option="--pattern=Typecheck"
```

### Run All Rule Engine Tests

```bash
cabal test bpc-core --test-option="--pattern=Rules"
```

### Test with SSOT Example

```bash
# The SSOT 8.4 example should parse and typecheck
echo 'field battery.capacity_kwh: Dec(6) = toDec(6, 50);' | cabal run bpc-core:parse-dsl
```

## Common Patterns

### Safe Field Access

```text
-- Get optional value with fallback
field battery.serial: Text =
  let b = getFact("Battery", "battery:SKU-123");
  unwrapOr(recordGet(unwrapOr(b, {}), "serial"), "UNKNOWN");
```

### Aggregation

```text
-- Sum all BOM weights
field bom.total_weight_kg: Qty(kg) =
  let parts = getFactsByPrefix("BOMPart", "bom:");
  let weights = map(parts, \p -> toQty("kg", recordGet(p, "weight")));
  sumQty("kg", weights);
```

### Conditional Logic

```text
-- Different calculation based on battery type
field battery.adjusted_capacity: Dec(6) =
  let b = requireSome(getFact("Battery", "battery:SKU-123"), "E001", "missing");
  let rawCap = toDec(6, recordGet(b, "capacity_kwh"));
  let batteryType = unwrapOr(recordGet(b, "type"), "standard");
  if (batteryType == "high-density") then
    rawCap * toDec(6, 1.1)
  else
    rawCap;
```

### Compliance Statement

```text
-- Emit compliance for EU Battery Regulation
field compliance.eu_battery_reg: Bool =
  let capacity = field("battery.capacity_kwh");
  let hasQR = isSome(field("qr.payload"));
  let compliant = capacity > 0 && hasQR;
  emitCompliance("EU-BR-2023-ANNEX-XIII", if compliant then "PASS" else "FAIL", "");
```

## Error Handling

### Parse Errors

```text
Parse error at line 5, column 12:
  unexpected '+'
  expecting ';' or operator
```

### Type Errors

```text
Type error at line 10, column 5:
  Cannot add Qty(kg) to Qty(kWh)
  Expected: Qty(kg)
  Actual: Qty(kWh)
```

### Cycle Errors

```text
Cycle detected in field dependencies:
  battery.capacity -> battery.adjusted -> battery.capacity
```

## Next Steps

After rule engine is complete:

1. **04-compilation-pipeline**: Uses evalExpr for passport compilation
2. **07-job-processing**: RUN_RULE_TESTS job uses test runner
