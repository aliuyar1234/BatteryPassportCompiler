# Feature Specification: Core Primitives

**Feature Branch**: `02-core-primitives`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 7.1-7.2 (Algorithmen), 5.5 (Core Type Signatures)
**Phase**: P1
**Package**: bpc-core

## User Scenarios & Testing

### User Story 1 - Canonical JSON Encoding (Priority: P1)

Als Compiler muss ich JSON-Werte deterministisch in kanonische Bytes umwandeln können, um byte-identische Hashes zu garantieren.

**Why this priority**: Determinismus ist NON-NEGOTIABLE Invariante (SSOT 0). Ohne kanonisches JSON funktioniert nichts.

**Independent Test**: Property Test `prop_canonicalEncode_deterministic` - gleicher Input ergibt immer gleiche Bytes.

**Acceptance Scenarios**:

1. **Given** JSON Object `{"b":2,"a":1}`, **When** `canonicalEncode` aufgerufen wird, **Then** ergibt sich `{"a":1,"b":2}` (keys sortiert nach UTF-8)
2. **Given** JSON mit Float `{"x":1.5}`, **When** `canonicalEncode` aufgerufen wird, **Then** wird `CanonicalNumberNotAllowed` Fehler zurückgegeben
3. **Given** JSON mit Exponentialzahl `{"x":1e10}`, **When** `canonicalEncode` aufgerufen wird, **Then** wird `CanonicalNumberNotAllowed` Fehler zurückgegeben
4. **Given** beliebiges JSON ohne Floats, **When** `canonicalEncode` zweimal aufgerufen wird, **Then** sind beide Ergebnisse byte-identisch

---

### User Story 2 - SHA-256 Hashing (Priority: P1)

Als System muss ich Bytes sicher hashen können für Integritätsprüfung und Tamper Detection.

**Why this priority**: Hash-Funktion ist Grundlage für Event Store, Proof, Receipt, Snapshot Hashing.

**Independent Test**: Hash-Fixtures aus SSOT 7.2 müssen exakt matchen.

**Acceptance Scenarios**:

1. **Given** Bytes `{"a":1,"b":2}`, **When** `sha256Hex` aufgerufen wird, **Then** ergibt sich `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777`
2. **Given** beliebige Bytes, **When** `sha256Hex` zweimal aufgerufen wird, **Then** sind beide Ergebnisse identisch
3. **Given** unterschiedliche Bytes, **When** `sha256Hex` auf beide aufgerufen wird, **Then** sind die Hashes unterschiedlich (Kollisionsresistenz)

---

### User Story 3 - Base32 Encoding (Priority: P2)

Als QR-Generator muss ich Hashes in Base32 ohne Padding kodieren können für platzsparende QR-Payloads.

**Why this priority**: Benötigt für BPC-QR-1 Format, aber nicht für initiale Core-Funktionalität.

**Independent Test**: Bekannte Bytes durch Base32 kodieren und Ergebnis prüfen.

**Acceptance Scenarios**:

1. **Given** beliebige Bytes, **When** `base32NoPad` aufgerufen wird, **Then** enthält das Ergebnis nur [A-Z2-7] Zeichen
2. **Given** beliebige Bytes, **When** `base32NoPad` aufgerufen wird, **Then** hat das Ergebnis kein `=` Padding
3. **Given** gleiche Bytes, **When** `base32NoPad` zweimal aufgerufen wird, **Then** sind beide Ergebnisse identisch

---

### User Story 4 - Domain Type Safety (Priority: P1)

Als Entwickler muss ich typsichere Wrapper für IDs und Werte haben um Verwechslungen zu verhindern.

**Why this priority**: Type Safety verhindert Bugs durch versehentliche ID-Verwechslung (z.B. TenantId vs PassportId).

**Independent Test**: Compiler verhindert Zuweisung von TenantId zu PassportId Variable.

**Acceptance Scenarios**:

1. **Given** `TenantId` Wert, **When** er einer `PassportId` Variable zugewiesen wird, **Then** schlägt Kompilierung fehl
2. **Given** `Dec(6)` Wert, **When** er mit `Dec(2)` Wert addiert wird, **Then** schlägt Typcheck fehl oder skaliert korrekt
3. **Given** `Qty("kg")` Wert, **When** er zu `Qty("kWh")` addiert wird, **Then** wird UNIT_MISMATCH Fehler geworfen

---

### User Story 5 - Unit Arithmetic (Priority: P2)

Als Rule Evaluator muss ich physikalische Einheiten korrekt rechnen können.

**Why this priority**: Unit Safety ist Teil der Rule DSL Typecheck Garantien (SSOT 8.3).

**Independent Test**: Unit-Konvertierung kg→g und Addition gleicher Units testen.

**Acceptance Scenarios**:

1. **Given** `Qty(1000, "g")`, **When** `convert("kg", qty)` aufgerufen wird, **Then** ergibt sich `Qty(1, "kg")`
2. **Given** zwei `Qty("kWh")` Werte, **When** sie addiert werden, **Then** ist das Ergebnis auch `Qty("kWh")`
3. **Given** `Qty("kg")` und `Qty("kWh")`, **When** Addition versucht wird, **Then** wird UNIT_MISMATCH Fehler geworfen
4. **Given** `Qty` und `Dec`, **When** multipliziert werden, **Then** ist das Ergebnis `Qty` mit gleicher Unit

---

### Edge Cases

- Was passiert bei Integer Overflow? → Haskell Integer ist unbounded; `Dec` verwendet Integer intern, daher kein Overflow
- Was passiert bei Unicode in JSON Strings? → UTF-8 Bytes werden korrekt sortiert
- Was passiert bei sehr tiefer JSON Nesting? → Stack-sichere Implementierung
- Was passiert bei `Dec` Division durch Null? → DIVISION_BY_ZERO Error (keine Exception)
- Was passiert bei sehr großen `Dec` Werten? → Kein Limit (Integer unbounded), aber Serialisierung prüft Max-Size

## Requirements

### Functional Requirements

- **FR-001**: `canonicalEncode` MUSS Object Keys nach UTF-8 Bytes sortieren [SSOT 7.1]
- **FR-002**: `canonicalEncode` MUSS Floats und Exponentialzahlen ablehnen [SSOT 7.1]
- **FR-003**: `canonicalEncode` MUSS Whitespace entfernen [SSOT 7.1]
- **FR-004**: `canonicalDecode` MUSS kanonische Bytes zurück zu Value parsen [SSOT 5.5]
- **FR-005**: `sha256Hex` MUSS SHA-256 Hash als lowercase Hex-String zurückgeben [SSOT 7.2]
- **FR-006**: `base32NoPad` MUSS RFC4648 Base32 ohne Padding (`=`) implementieren; Output enthält nur `[A-Z2-7]}` (standard Base32 alphabet, no padding per QR requirements) [SSOT 7.8]
- **FR-007**: Alle Domain IDs MÜSSEN als newtype Wrapper implementiert sein [SSOT 5.5]
- **FR-008**: `Dec(scale)` MUSS Decimal mit fester Nachkommastellen-Anzahl repräsentieren
- **FR-009**: `Qty(unit)` MUSS Menge mit physikalischer Einheit repräsentieren
- **FR-010**: Unit Arithmetik MUSS SSOT 8.3 Regeln folgen: `+/-` nur gleiche Unit, `*` Qty×Dec, `/` Qty÷Dec
- **FR-011**: `nodeHash` MUSS Proof Node Hashes nach BPC-PROOF-HASH-1 berechnen [SSOT 9.2]

### Key Entities

- **Value**: Aeson JSON Value (Null, Bool, Number, String, Array, Object)
- **CanonicalBytes**: ByteString mit garantiert kanonischem Inhalt
- **Hash**: 64-Zeichen lowercase Hex SHA-256 Hash
- **TenantId, PassportId, SnapshotId, etc.**: UUID-basierte newtype Wrapper
- **Dec(scale)**: Decimal mit fester Precision
- **Qty(unit)**: Physikalische Größe mit Einheit

### Type Signatures (SSOT 5.5)

```haskell
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalDecode :: ByteString -> Either CanonicalError Value

sha256Hex :: ByteString -> Text
base32NoPad :: ByteString -> Text
nodeHash :: NodeId -> NodeType -> Value -> [Text] -> Text  -- Hash for proof nodes [SSOT 9.2]

-- Domain IDs
newtype TenantId = TenantId UUID
newtype PassportId = PassportId UUID
newtype SnapshotId = SnapshotId UUID
newtype DocumentId = DocumentId UUID
newtype DocumentVersionId = DocumentVersionId UUID
newtype FactId = FactId UUID
newtype RulePackageId = RulePackageId UUID
newtype RulePackageVersionId = RulePackageVersionId UUID
newtype PassportVersionId = PassportVersionId UUID
newtype EventId = EventId UUID
newtype JobId = JobId UUID
newtype ActorId = ActorId UUID
newtype ApiKeyId = ApiKeyId UUID

-- Decimal/Quantity
newtype Dec (scale :: Nat) = Dec Integer
data Qty (unit :: Symbol) = Qty (Dec 6)
```

### Supported Units (SSOT 8.1)

| Unit | Beschreibung |
|------|--------------|
| kg | Kilogramm |
| g | Gramm |
| kWh | Kilowattstunden |
| Wh | Wattstunden |
| gCO2e | Gramm CO2-Äquivalent |
| kgCO2e | Kilogramm CO2-Äquivalent |
| gCO2e_per_kWh | Emissionsintensität |
| pct | Prozent |
| each | Stückzahl |

## Success Criteria

### Measurable Outcomes

- **SC-001**: Property Test `prop_canonicalEncode_deterministic` besteht mit 10.000 Samples
- **SC-002**: Alle Hash-Fixtures aus SSOT 7.2 werden exakt reproduziert
- **SC-003**: Unit Tests decken alle 9 unterstützten Einheiten und deren Konvertierungen ab
- **SC-004**: Code Coverage für BPC.Core.CanonicalJson >= 95%
- **SC-005**: Code Coverage für BPC.Core.Hash >= 95%
- **SC-006**: Keine IO-Imports in bpc-core (Dependency Rule SSOT 3.4)
