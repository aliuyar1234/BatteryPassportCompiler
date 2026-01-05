# Feature Specification: Rule Engine (DSL)

**Feature Branch**: `03-rule-engine`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 8 (Rule-DSL BPC-RULES-1), 7.5 (Topo Sort)
**Phase**: P1
**Package**: bpc-core

## User Scenarios & Testing

### User Story 1 - DSL Parsing (Priority: P1)

Als System muss ich Rule DSL Source Code in einen AST parsen können.

**Why this priority**: Ohne Parser kann keine Rule interpretiert werden. Dies ist die Basis für den gesamten Compiler.

**Independent Test**: SSOT 8.4 Beispiel-DSL parsen und AST-Struktur validieren.

**Acceptance Scenarios**:

1. **Given** gültiger DSL Source `field battery.capacity_kwh: Dec(6) = ...;`, **When** Parser aufgerufen wird, **Then** wird ein valider untyped AST zurückgegeben
2. **Given** DSL mit Syntaxfehler (fehlende Semikolon), **When** Parser aufgerufen wird, **Then** wird RULE_PARSE_ERROR mit Zeilennummer zurückgegeben
3. **Given** DSL mit let-Expression, **When** Parser aufgerufen wird, **Then** wird verschachtelte LetExpr im AST erzeugt
4. **Given** DSL mit Kommentar `-- comment`, **When** Parser aufgerufen wird, **Then** wird Kommentar ignoriert

---

### User Story 2 - Type Checking (Priority: P1)

Als System muss ich den untyped AST in einen typed AST (GADTs) transformieren und Typfehler erkennen.

**Why this priority**: Typecheck garantiert, dass zur Laufzeit keine Typfehler auftreten können (SSOT 8.3).

**Independent Test**: Unit-Test mit absichtlich falsch getypter Rule.

**Acceptance Scenarios**:

1. **Given** untyped AST mit `Dec(6) + Dec(2)`, **When** Typecheck aufgerufen wird, **Then** wird RULE_TYPE_ERROR oder automatische Skalierung durchgeführt
2. **Given** untyped AST mit `Qty("kg") + Qty("kWh")`, **When** Typecheck aufgerufen wird, **Then** wird UNIT_MISMATCH Error zurückgegeben
3. **Given** untyped AST mit unbekanntem field-Ref `field("nonexistent.field")`, **When** Typecheck aufgerufen wird, **Then** wird RULE_TYPE_ERROR zurückgegeben
4. **Given** korrekter untyped AST, **When** Typecheck aufgerufen wird, **Then** wird typed `Expr` (GADT) zurückgegeben

---

### User Story 3 - Dependency Graph & Topo Sort (Priority: P1)

Als Compiler muss ich die Reihenfolge der Feld-Evaluierung bestimmen können basierend auf Abhängigkeiten.

**Why this priority**: Felder können andere Felder referenzieren. Ohne korrekte Sortierung ist Evaluation unmöglich.

**Independent Test**: Property Test - nach TopoSort sind alle Abhängigkeiten vor dem abhängigen Feld.

**Acceptance Scenarios**:

1. **Given** Rules wo `field A` von `field B` abhängt, **When** `topoSortFields` aufgerufen wird, **Then** kommt B vor A in der Liste
2. **Given** zyklische Abhängigkeit `A -> B -> A`, **When** `topoSortFields` aufgerufen wird, **Then** wird RULE_CYCLE_DETECTED mit cycle_fields zurückgegeben
3. **Given** unabhängige Felder, **When** `topoSortFields` aufgerufen wird, **Then** werden sie in lexikalischer Reihenfolge sortiert (tie-break)
4. **Given** komplexer Abhängigkeitsgraph, **When** `topoSortFields` aufgerufen wird, **Then** ist jedes Feld nach allen seinen Abhängigkeiten

---

### User Story 4 - Built-in Functions (Priority: P1)

Als Rule-Autor muss ich vordefinierte Funktionen nutzen können um Facts abzufragen und Werte zu transformieren.

**Why this priority**: Built-ins sind die API zwischen DSL und Daten. Ohne sie ist die DSL nutzlos.

**Independent Test**: Jede Built-in Funktion einzeln testen mit Mock-Facts.

**Acceptance Scenarios**:

1. **Given** `getFact("Battery", "battery:SKU-123")`, **When** evaluiert wird, **Then** wird `Opt(Record)` mit Fact-Daten oder `none` zurückgegeben
2. **Given** `requireSome(none, "E001", "missing")`, **When** evaluiert wird, **Then** wird Error mit Code E001 und Message geworfen
3. **Given** `toDec(6, someInt)`, **When** evaluiert wird, **Then** wird Integer zu Dec(6) konvertiert
4. **Given** `convert("kg", "g", qty(1000, "g"))`, **When** evaluiert wird, **Then** wird `qty(1, "kg")` zurückgegeben
5. **Given** `sumQty("kWh", [qty(1,"kWh"), qty(2,"kWh")])`, **When** evaluiert wird, **Then** wird `qty(3, "kWh")` zurückgegeben

---

### User Story 5 - Example Tests (Priority: P2)

Als Rule-Autor muss ich Example Tests definieren können um konkrete Szenarien zu validieren.

**Why this priority**: Tests ermöglichen Qualitätssicherung vor Publish, aber nicht für initiale Parsing/Eval notwendig.

**Independent Test**: Example Test aus DSL parsen und gegen Mock-Facts ausführen.

**Acceptance Scenarios**:

1. **Given** DSL mit `example test1: { fact ... } => expect(...);`, **When** Test-Parser aufgerufen wird, **Then** wird Test-Definition extrahiert
2. **Given** Example Test mit korrekten Facts, **When** Test ausgeführt wird, **Then** passiert er (PASSED)
3. **Given** Example Test mit falschem erwarteten Wert, **When** Test ausgeführt wird, **Then** schlägt er fehl (FAILED) mit Details

---

### User Story 6 - Property Tests (Priority: P3)

Als Rule-Autor muss ich Property Tests definieren können für generalisierte Validierung.

**Why this priority**: Property Tests sind fortgeschrittenes Feature für robuste Rule-Validierung.

**Independent Test**: Property Test aus DSL parsen und mit Generator ausführen.

**Acceptance Scenarios**:

1. **Given** DSL mit `property prop1: cases(500) seed(42) => forall x: Dec(6). implies(...);`, **When** Test ausgeführt wird, **Then** werden 500 Testfälle generiert
2. **Given** Property Test der immer gilt, **When** mit beliebigem Seed ausgeführt wird, **Then** passiert er
3. **Given** Property Test der manchmal fehlschlägt, **When** ausgeführt wird, **Then** wird Gegenbeispiel zurückgegeben

---

### Edge Cases

- Was passiert bei leerer DSL Source? → Leere Field-Liste, aber kein Fehler
- Was passiert bei sehr langen Feldpfaden? → Keine künstliche Begrenzung
- Was passiert bei Division durch Zero? → DIVISION_BY_ZERO Error
- Wie verhält sich `unwrapOr(none, default)`? → Gibt `default` zurück

## Requirements

### Functional Requirements

- **FR-001**: Parser MUSS vollständige EBNF aus SSOT 8.1 implementieren
- **FR-002**: Parser MUSS `-- comment` Zeilen ignorieren
- **FR-003**: Parser MUSS RULE_PARSE_ERROR mit Zeile/Spalte/Offset bei Syntaxfehlern zurückgeben (erste Fehlerposition, keine Error Recovery)
- **FR-004**: Typecheck MUSS untyped AST in typed AST (GADTs) transformieren [SSOT 8.3]
- **FR-005**: Typecheck MUSS RULE_TYPE_ERROR bei Typfehlern zurückgeben
- **FR-006**: Typecheck MUSS UNIT_MISMATCH bei inkompatiblen Units zurückgeben
- **FR-007**: `topoSortFields` MUSS Kahn-Algorithmus mit lexikalischem Tie-Break verwenden [SSOT 7.5]
- **FR-008**: `topoSortFields` MUSS RULE_CYCLE_DETECTED mit cycle_fields bei Zyklen zurückgeben
- **FR-009**: Alle 16 Built-in Functions aus SSOT 8.2 MÜSSEN implementiert sein (getFact, getFactsByPrefix, field, recordGet, isSome, unwrapOr, requireSome, toDec, toQty, convert, sumQty, sumDec, map, filter, fold, emitCompliance)
- **FR-010**: Example + Property Tests MÜSSEN zusammen ≥500 Testfälle pro RulePackageVersion haben bevor Publish erlaubt ist; `cases < 500` triggert RULE_TESTS_FAILED [SSOT 2.4]
- **FR-011**: Property Tests MÜSSEN mit angegebenem Seed reproduzierbar sein

### Key Entities

- **Source**: DSL Quellcode als Text
- **UntypedAST**: Parse-Ergebnis vor Typecheck (siehe `data-model.md` für alle Konstruktoren)
- **TypedExpr**: GADT-basierter typsicherer AST (Konstruktoren: Lit, Var, Let, If, BinOp, UnaryOp, FuncCall, FieldRef, RecordGet, ListOp, etc. - siehe `data-model.md`)
- **FieldPath**: Pfad wie `battery.capacity_kwh`
- **ExampleTest**: Konkreter Test mit Fixtures und Assertions
- **PropertyTest**: Generator-basierter Test mit forall-Quantor

*Vollständige Haskell-Typdefinitionen: `specs/03-rule-engine/data-model.md`*

### Built-in Functions (SSOT 8.2)

| Function | Signatur | Beschreibung |
|----------|----------|--------------|
| getFact | (Text,Text) -> Opt(Record) | Fact nach Type/Key laden |
| getFactsByPrefix | (Text,Text) -> List(Record) | Facts nach Prefix laden |
| field | (Text) -> any | Anderes Feld referenzieren |
| recordGet | (Record,Text) -> Opt(any) | Feld aus Record extrahieren |
| isSome | Opt(a) -> Bool | Option prüfen |
| unwrapOr | (Opt(a),a) -> a | Option mit Default entpacken |
| requireSome | (Opt(a),Text,Text) -> a | Option mit Fehler entpacken |
| toDec | (Int,Int\|Dec) -> Dec(scale) | Zu Decimal konvertieren |
| toQty | (Text,Dec\|Int) -> Qty(unit) | Zu Quantity konvertieren |
| convert | (Text,Text,Qty(u)) -> Qty(v) | Unit konvertieren |
| sumQty | (Text,List(Qty(u))) -> Qty(u) | Quantities summieren |
| sumDec | (Int,List(Dec(_))) -> Dec(scale) | Decimals summieren |
| map | (List(a), (a->b)) -> List(b) | Liste transformieren |
| filter | (List(a), (a->Bool)) -> List(a) | Liste filtern |
| fold | (List(a), b, (b,a->b)) -> b | Liste reduzieren |
| emitCompliance | (Text,Text,Text) -> Bool | Compliance-Statement emittieren |

### Type System Rules (SSOT 8.3)

- `+/-`: Nur gleiche Unit/Scale erlaubt
- `*`: Qty × Dec → Qty
- `/`: Qty ÷ Dec → Qty
- Vergleiche (`==`, `<`, etc.): Nur kompatible Typen

## Success Criteria

### Measurable Outcomes

- **SC-001**: SSOT 8.4 Beispiel-DSL parst und typecheckt erfolgreich
- **SC-002**: Property Test `forall input. parse(typecheck(input)) or RULE_TYPE_ERROR` besteht
- **SC-003**: Alle 16 Built-in Functions haben Unit Tests
- **SC-004**: Code Coverage für BPC.Core.Rules.* >= 90%
- **SC-005**: Parser Performance: 10.000 Zeilen DSL in < 1 Sekunde
- **SC-006**: Keine IO-Imports in bpc-core (Dependency Rule SSOT 3.4)
