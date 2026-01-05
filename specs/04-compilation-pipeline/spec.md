# Feature Specification: Compilation Pipeline

**Feature Branch**: `04-compilation-pipeline`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 7.6 (BPC-COMPILE-1), 9 (Proof & Receipt & Replay), 7.8 (QR)
**Phase**: P1
**Package**: bpc-core

## User Scenarios & Testing

### User Story 1 - Pure Passport Compilation (Priority: P1)

Als System muss ich aus Snapshot + Rules deterministisch ein Passport mit Payload, Proof und Receipt kompilieren können.

**Why this priority**: Dies ist der Kern des gesamten Systems - die pure Compilation-Funktion. NON-NEGOTIABLE Determinismus-Garantie.

**Independent Test**: `compilePassportPure` mit gleichen Inputs muss byte-identische Outputs liefern.

**Acceptance Scenarios**:

1. **Given** CompileInput (Snapshot, Rules, BatteryProduct, IssuedAt, BuildId), **When** `compilePassportPure` aufgerufen wird, **Then** wird `Either CompileError CompileOutput` zurückgegeben
2. **Given** identischer CompileInput, **When** `compilePassportPure` zweimal aufgerufen wird, **Then** sind payload_hash, proof_root_hash, receipt_hash byte-identisch
3. **Given** Snapshot mit fehlenden Facts für Required Fields, **When** compiliert wird, **Then** wird entsprechender Error mit Field-Info zurückgegeben
4. **Given** Rules mit DIVISION_BY_ZERO, **When** compiliert wird, **Then** wird DIVISION_BY_ZERO Error mit Kontext zurückgegeben

---

### User Story 2 - Proof Tree Generation (Priority: P1)

Als System muss ich einen Derivation Tree (Proof) generieren der jeden Rechenschritt nachweist.

**Why this priority**: Der Proof ermöglicht Audit-Replay und Nachvollziehbarkeit aller Berechnungen.

**Independent Test**: Proof für einfache Rule generieren und Struktur validieren.

**Acceptance Scenarios**:

1. **Given** kompiliertes Passport, **When** Proof extrahiert wird, **Then** enthält er `proof_version: "BPC-PROOF-1"`, `root_hash`, `nodes`, `field_index`
2. **Given** Proof Node, **When** `node.hash` berechnet wird nach BPC-PROOF-HASH-1, **Then** stimmt es mit gespeichertem Hash überein
3. **Given** Proof mit mehreren Nodes, **When** `verifyProof` aufgerufen wird, **Then** werden alle Node-Hashes verifiziert und `Right ()` zurückgegeben
4. **Given** manipulierter Proof (falscher Hash), **When** `verifyProof` aufgerufen wird, **Then** wird `Left ProofError` zurückgegeben

---

### User Story 3 - Receipt Generation (Priority: P1)

Als System muss ich einen maschinenprüfbaren Receipt generieren der alle Input/Output Hashes enthält.

**Why this priority**: Receipt ist der Nachweis der Compilation - enthält alle Hashes für Replay-Verifikation.

**Independent Test**: Receipt für kompiliertes Passport generieren und Felder validieren.

**Acceptance Scenarios**:

1. **Given** kompiliertes Passport, **When** Receipt extrahiert wird, **Then** enthält er alle Felder aus BPC-RECEIPT-1 Schema
2. **Given** Receipt, **When** Hashes verglichen werden, **Then** stimmen `snapshot_hash`, `payload_hash`, `proof_root_hash` mit berechneten Werten überein
3. **Given** unsigned Receipt, **When** `hashReceiptUnsigned` aufgerufen wird, **Then** wird deterministischer Hash zurückgegeben
4. **Given** Receipt ohne Signatur, **When** serialisiert wird, **Then** ist `signature` Feld nicht vorhanden

---

### User Story 4 - Signature Generation & Verification (Priority: P1)

Als System muss ich den Receipt mit ED25519 signieren und Signaturen verifizieren können.

**Why this priority**: Signatur ist der kryptographische Beweis der Authentizität.

**Independent Test**: Receipt signieren und Signatur verifizieren.

**Acceptance Scenarios**:

1. **Given** Receipt Hash und ED25519 Private Key, **When** `signReceiptHash` aufgerufen wird, **Then** wird gültige Signatur zurückgegeben
2. **Given** signierter Receipt, **When** `ED25519.verify(pubKey, hash, signature)` aufgerufen wird, **Then** wird `True` zurückgegeben
3. **Given** signierter Receipt mit falscher Signatur, **When** verify aufgerufen wird, **Then** wird `False` zurückgegeben
4. **Given** kein Signing Key konfiguriert, **When** Signatur versucht wird, **Then** wird SIGNING_KEY_MISSING Error geworfen

---

### User Story 5 - QR Payload Generation (Priority: P2)

Als System muss ich einen platzsparenden QR-Payload String generieren.

**Why this priority**: QR ist für physische Batterie-Labels, aber nicht für digitale Passport-Nutzung kritisch.

**Independent Test**: QR Payload generieren und Format validieren.

**Acceptance Scenarios**:

1. **Given** PassportVersion mit IDs und Hashes, **When** `buildQrPayload` aufgerufen wird, **Then** wird String im Format `BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>` zurückgegeben
2. **Given** QR Payload, **When** Base32 Hashes dekodiert werden, **Then** stimmen sie mit Original-Hashes überein
3. **Given** beliebiger QR Input, **When** `buildQrPayload` zweimal aufgerufen wird, **Then** sind beide Ergebnisse identisch

---

### User Story 6 - Size Limit Enforcement (Priority: P1)

Als System muss ich Größenlimits für Artifacts einhalten.

**Why this priority**: Ohne Limits könnten unbegrenzt große Artifacts entstehen die Storage/Memory sprengen.

**Independent Test**: Sehr große Rule compilieren und auf Limit-Error prüfen.

**Acceptance Scenarios**:

1. **Given** Compilation die > 131,072 Bytes Payload erzeugen würde, **When** compiliert wird, **Then** wird PAYLOAD_TOO_LARGE Error zurückgegeben
2. **Given** Compilation die > 262,144 Bytes Proof erzeugen würde, **When** compiliert wird, **Then** wird PROOF_TOO_LARGE Error zurückgegeben
3. **Given** Compilation die > 16,384 Bytes Receipt erzeugen würde, **When** compiliert wird, **Then** wird RECEIPT_TOO_LARGE Error zurückgegeben
4. **Given** Compilation unter allen Limits, **When** compiliert wird, **Then** wird erfolgreich CompileOutput zurückgegeben

---

### User Story 7 - Golden Tests (Priority: P2)

Als Entwickler muss ich byte-exakte Regression Tests haben für Determinismus-Garantie.

**Why this priority**: Golden Tests beweisen, dass sich Outputs nicht unbeabsichtigt ändern.

**Independent Test**: Fixtures gegen aktuelle Compilation vergleichen.

**Acceptance Scenarios**:

1. **Given** Fixture `minimal.json` mit bekanntem Input/Output, **When** neu kompiliert wird, **Then** sind alle canonical Bytes identisch
2. **Given** Fixture `medium.json` mit mehreren Fields, **When** neu kompiliert wird, **Then** sind alle Hashes identisch
3. **Given** Fixture `edge.json` mit Randfällen, **When** neu kompiliert wird, **Then** sind alle Bytes identisch

---

### Edge Cases

- Was passiert bei leeren Rules? → Leeres Payload, minimaler Proof
- Was passiert bei sehr vielen Fields? → Performance-Test, aber kein künstliches Limit (Size Limits gelten)
- Wie verhält sich Compilation bei bereits existierenden Hashes? → Pure Function, keine DB-Interaktion
- Was passiert bei sehr tiefen Proof Trees? → Keine künstliche Tiefenbegrenzung, aber PROOF_TOO_LARGE bei > 262,144 bytes
- Was ist das BuildId Format? → Text aus `BPC_COMPILER_BUILD_ID` ENV, Default `"UNKNOWN"`, typisch Git-SHA oder Semver

## Requirements

### Functional Requirements

- **FR-001**: `compilePassportPure` MUSS pure sein (keine IO) [SSOT 5.5, Constitution V]
- **FR-002**: `compilePassportPure` MUSS deterministisch sein - gleiche Inputs → byte-identische Outputs [SSOT 0]
- **FR-003**: Compilation MUSS Rules in topologischer Reihenfolge evaluieren [SSOT 7.5]
- **FR-004**: Proof MUSS BPC-PROOF-1 Schema exakt implementieren [SSOT 9.1]
- **FR-005**: Node Hash MUSS nach BPC-PROOF-HASH-1 berechnet werden [SSOT 9.2]
- **FR-006**: Receipt MUSS BPC-RECEIPT-1 Schema exakt implementieren [SSOT 9.3]
- **FR-007**: Signatur MUSS ED25519 über Receipt Hash sein [SSOT 7.7]
- **FR-008**: QR Payload MUSS BPC-QR-1 Format implementieren [SSOT 7.8]
- **FR-009**: Size Limits: Payload ≤ 131,072 / Proof ≤ 262,144 / Receipt ≤ 16,384 Bytes [SSOT 7.6]
- **FR-010**: Golden Tests MÜSSEN >= 3 Fixtures mit byte-exaktem Vergleich haben [SSOT 13.4]
- **FR-011**: BuildId MUSS non-empty Text sein; Source: `BPC_COMPILER_BUILD_ID` ENV, Default: `"UNKNOWN"`, typisch: Git-SHA oder Semver [SSOT 4.6]

### Key Entities

- **CompileInput**: Snapshot + Rules + BatteryProduct + IssuedAt + BuildId (Format: ENV `BPC_COMPILER_BUILD_ID`, Default: `"UNKNOWN"`, typisch: Git-Hash oder Semver)
- **CompileOutput**: Payload + PayloadHash + Proof + ProofRootHash + ReceiptUnsigned + ReceiptHash
- **Proof (BPC-PROOF-1)**: Derivation Tree mit Nodes und field_index (keine künstliche Tiefenbegrenzung, rekursive Struktur)
- **ProofNode**: id, type, hash, data, children
- **Receipt (BPC-RECEIPT-1)**: Alle Hashes + Metadata + Signatur
- **QrPayload**: Kompakter String für physische Labels

### Type Signatures (SSOT 5.5)

```haskell
compilePassportPure :: CompileInput -> Either CompileError CompileOutput

verifyProof :: ByteString -> Either ProofError ()

buildReceiptUnsigned :: ReceiptInput -> ReceiptUnsigned
hashReceiptUnsigned :: ReceiptUnsigned -> ReceiptHash
signReceiptHash :: Ed25519PrivateKey -> ReceiptHash -> Signature

buildQrPayload :: QrInput -> Text
```

### Proof Node Types (SSOT 9.1)

| Type | Beschreibung |
|------|--------------|
| CONST | Konstanter Wert |
| FACT_GET | Fact-Abfrage |
| FIELD_REF | Feld-Referenz |
| OP | Operation (+, -, *, /, etc.) |
| ASSERT | Assertion/RequireSome |
| COMPLIANCE_EMIT | emitCompliance Aufruf |

### Size Limits (SSOT 7.6)

| Artifact | Max Size | Error Code |
|----------|----------|------------|
| Payload | 131,072 bytes | PAYLOAD_TOO_LARGE |
| Proof | 262,144 bytes | PROOF_TOO_LARGE |
| Receipt | 16,384 bytes | RECEIPT_TOO_LARGE |

## Success Criteria

### Measurable Outcomes

- **SC-001**: Property Test `prop_compile_deterministic` besteht mit 1.000 Samples
- **SC-002**: Alle 3+ Golden Fixtures sind byte-exakt
- **SC-003**: `verifyProof` akzeptiert alle von `compilePassportPure` generierten Proofs
- **SC-004**: Signatur-Roundtrip (sign → verify) funktioniert immer
- **SC-005**: Code Coverage für BPC.Core.Eval, Proof, Receipt >= 90%
- **SC-006**: Keine IO-Imports in bpc-core (Dependency Rule SSOT 3.4)
