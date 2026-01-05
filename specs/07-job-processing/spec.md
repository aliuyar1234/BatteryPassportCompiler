# Feature Specification: Job Processing (Worker)

**Feature Branch**: `07-job-processing`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 3.5.4 (bpc-worker Exports), 14.3-14.5 (Tasks P1-P3)
**Phase**: P2-P3
**Package**: bpc-worker

## User Scenarios & Testing

### User Story 1 - Worker Loop (Priority: P1)

Als System muss ich Jobs aus der Queue kontinuierlich verarbeiten.

**Why this priority**: Worker Loop ist die Basis für alle async Verarbeitung.

**Independent Test**: Job enqueuen und Worker-Verarbeitung beobachten.

**Acceptance Scenarios**:

1. **Given** Worker gestartet, **When** Jobs in Queue sind, **Then** werden sie sequentiell verarbeitet
2. **Given** Worker mit Lease, **When** Lease ablaufen würde, **Then** wird Lease automatisch renewed
3. **Given** kein Queue (BPC_QUEUE_ENABLED=false), **When** Worker läuft, **Then** pollt er DB alle `BPC_JOBS_POLL_INTERVAL_MS`
4. **Given** RabbitMQ enabled, **When** Job enqueued wird, **Then** triggert Queue-Message den Worker sofort

---

### User Story 2 - Job Leasing & Locking (Priority: P1)

Als System muss ich garantieren, dass Jobs nicht doppelt verarbeitet werden.

**Why this priority**: Race Conditions bei Job-Verarbeitung würden zu Duplikaten und Inkonsistenzen führen.

**Independent Test**: Mehrere Worker parallel starten und prüfen, dass jeder Job nur einmal läuft.

**Acceptance Scenarios**:

1. **Given** Job QUEUED, **When** `acquireLease` aufgerufen wird, **Then** wird Job RUNNING mit lease_owner und lease_expires_at
2. **Given** Job bereits geleased, **When** anderer Worker Lease versucht, **Then** wird Job nicht zurückgegeben
3. **Given** abgelaufener Lease, **When** `acquireLease` aufgerufen wird, **Then** kann neuer Worker den Job übernehmen
4. **Given** Worker-Crash vor Completion, **When** Lease abläuft, **Then** wird Job erneut verfügbar

---

### User Story 3 - Error Handling & Retries (Priority: P1)

Als System muss ich fehlgeschlagene Jobs mit Backoff wiederholen.

**Why this priority**: Transiente Fehler (DB, Network) sollen automatisch geheilt werden.

**Independent Test**: Job zum Scheitern bringen und Retry-Verhalten prüfen.

**Acceptance Scenarios**:

1. **Given** Job schlägt fehl mit retryable Error, **When** attempts < max_attempts, **Then** wird Job re-queued mit exponentialem Backoff
2. **Given** Job mit attempts >= max_attempts, **When** erneut fehlschlägt, **Then** wird Status DEAD_LETTER
3. **Given** Job mit non-retryable Error, **When** fehlschlägt, **Then** wird Status direkt FAILED
4. **Given** fehlgeschlagener Job, **When** Status gesetzt wird, **Then** wird last_error mit Details gespeichert

---

### User Story 4 - PARSE_FACTS Handler (Priority: P1)

Als System muss ich DocumentVersions parsen und Facts extrahieren.

**Why this priority**: Facts sind die Basis für Snapshots - ohne Parsing keine Daten.

**Independent Test**: Document uploaden und Fact-Generierung prüfen.

**Acceptance Scenarios**:

1. **Given** DocumentVersion UPLOADED mit BOM-Content, **When** PARSE_FACTS Job läuft, **Then** werden Facts mit Type "BOM" extrahiert
2. **Given** geparste Facts, **When** gespeichert werden, **Then** haben sie `payload_canonical` und `payload_hash`
3. **Given** erfolgreiches Parsing, **When** Job abgeschlossen ist, **Then** wird DocumentVersion Status VALIDATED
4. **Given** ungültiges Document-Format, **When** Parsing fehlschlägt, **Then** wird Status REJECTED mit Error-Details

---

### User Story 5 - BUILD_SNAPSHOT Handler (Priority: P1)

Als System muss ich Snapshots von BUILDING zu READY überführen.

**Why this priority**: Nur READY Snapshots können versiegelt werden.

**Independent Test**: Snapshot mit Facts erstellen und BUILD Job ausführen.

**Acceptance Scenarios**:

1. **Given** Snapshot BUILDING mit Items, **When** BUILD_SNAPSHOT Job läuft, **Then** wird Status READY
2. **Given** Snapshot ohne Items, **When** BUILD Job läuft, **Then** ist Snapshot trotzdem READY (leer ist valid)

---

### User Story 6 - COMPILE_PASSPORT Handler (Priority: P1)

Als System muss ich Passports aus Snapshot und Rules kompilieren.

**Why this priority**: Compilation ist der Kern des gesamten Systems.

**Independent Test**: Sealed Snapshot + Published Rules → PassportVersion mit Artifacts.

**Acceptance Scenarios**:

1. **Given** SEALED Snapshot + PUBLISHED Rules, **When** COMPILE_PASSPORT Job läuft, **Then** wird PassportVersion mit payload/proof/receipt erstellt
2. **Given** Snapshot nicht SEALED, **When** Compile versucht wird, **Then** wird SNAPSHOT_NOT_READY Error
3. **Given** Rules nicht PUBLISHED, **When** Compile versucht wird, **Then** wird RULE_PKG_NOT_PUBLISHED Error
4. **Given** erfolgreiches Compile, **When** abgeschlossen, **Then** wird SIGN_PASSPORT Job enqueued

---

### User Story 7 - SIGN_PASSPORT Handler (Priority: P1)

Als System muss ich den Receipt mit ED25519 signieren.

**Why this priority**: Signatur ist der kryptographische Authentizitätsnachweis.

**Independent Test**: PassportVersion COMPILING → SIGNED mit Signatur.

**Acceptance Scenarios**:

1. **Given** PassportVersion mit Receipt, **When** SIGN_PASSPORT Job läuft, **Then** wird `signature` mit ED25519 berechnet
2. **Given** kein Signing Key konfiguriert, **When** Sign versucht wird, **Then** wird SIGNING_KEY_MISSING Error
3. **Given** erfolgreiches Signing, **When** abgeschlossen, **Then** wird Status SIGNED und GENERATE_QR Job enqueued

---

### User Story 8 - GENERATE_QR Handler (Priority: P2)

Als System muss ich QR Code PNG generieren.

**Why this priority**: QR ist für physische Labels, nicht für digitale Nutzung kritisch.

**Independent Test**: PassportVersion SIGNED → QR PNG generiert.

**Acceptance Scenarios**:

1. **Given** PassportVersion SIGNED, **When** GENERATE_QR Job läuft, **Then** wird `qr_png` Bytes und `qr_payload` String gespeichert
2. **Given** QR Payload, **When** gescannt wird, **Then** enthält er Format `BPC1|pv=...|ph=...|pr=...|rh=...`
3. **Given** erfolgreiches QR, **When** abgeschlossen, **Then** bleibt Status SIGNED (kein Statuswechsel)

---

### User Story 9 - EXPORT_PASSPORT Handler (Priority: P3)

Als System muss ich Passports in verschiedene Formate exportieren können.

**Why this priority**: Export ist optionales Feature für Datenaustauschs-Szenarien.

**Independent Test**: PassportVersion exportieren und Export-File prüfen.

**Acceptance Scenarios**:

1. **Given** PassportVersion ACTIVE, **When** EXPORT_PASSPORT Job mit format=JSON läuft, **Then** wird JSON Export-File generiert
2. **Given** PassportVersion ACTIVE, **When** EXPORT_PASSPORT Job mit format=PDF läuft, **Then** wird PDF Export-File generiert (optional, P4+)
3. **Given** Export erfolgreich, **When** abgeschlossen, **Then** wird Export-File Path im Job Result gespeichert
4. **Given** PassportVersion nicht ACTIVE, **When** Export versucht wird, **Then** wird Error zurückgegeben

---

### User Story 10 - RUN_RULE_TESTS Handler (Priority: P2)

Als System muss ich Rule Tests ausführen bevor Publish erlaubt wird.

**Why this priority**: Tests garantieren Rule-Qualität vor Publish.

**Independent Test**: RulePackageVersion mit Tests → Test Run Ergebnis.

**Acceptance Scenarios**:

1. **Given** RulePackageVersion mit Example Tests, **When** RUN_RULE_TESTS Job läuft, **Then** werden alle Tests ausgeführt
2. **Given** >= 500 Testfälle PASSED, **When** abgeschlossen, **Then** wird Status VALIDATED
3. **Given** Tests FAILED oder < 500 cases, **When** abgeschlossen, **Then** bleibt Status DRAFT mit Failure-Details

---

### User Story 11 - DELIVER_WEBHOOK Handler (Priority: P3)

Als System muss ich Webhook-Payloads an konfigurierte Endpoints senden.

**Why this priority**: Webhooks sind optionales Integrationsfeature.

**Independent Test**: Event triggern und Webhook Delivery prüfen.

**Acceptance Scenarios**:

1. **Given** Webhook Subscription für Event Type, **When** Event auftritt, **Then** wird DELIVER_WEBHOOK Job erstellt
2. **Given** Webhook Delivery, **When** Endpoint erreichbar, **Then** wird Request mit X-BPC-Signature Header gesendet
3. **Given** Endpoint nicht erreichbar, **When** Delivery fehlschlägt, **Then** wird mit Backoff wiederholt
4. **Given** erfolgreiche Delivery, **When** abgeschlossen, **Then** wird Status DELIVERED mit delivered_at

---

### Edge Cases

- Was passiert wenn Worker während Job-Verarbeitung crasht? → Lease läuft ab, Job wird von anderem Worker übernommen
- Was passiert bei DB-Ausfall während Job? → Job bleibt RUNNING bis Lease abläuft, dann Retry
- Wie verhält sich Priority? → Jobs mit höherer Priority (niedrigere Zahl) werden zuerst verarbeitet
- Was passiert bei DEAD_LETTER Jobs? → Manuelle Intervention erforderlich; Monitoring Alert auslösen; `last_error` enthält Details
- Was sind die Queue Namen? → Exchange: `bpc`, Routing Key: `jobs.trigger` (per ENV konfigurierbar)

## Requirements

### Functional Requirements

- **FR-001**: Worker MUSS Jobs per DB Polling oder RabbitMQ Trigger verarbeiten [SSOT 4.6]
- **FR-001b**: RabbitMQ Config: Exchange `BPC_QUEUE_EXCHANGE=bpc`, Routing Key `BPC_QUEUE_ROUTING_KEY=jobs.trigger` [SSOT 4.6]
- **FR-002**: Worker MUSS Lease mit `BPC_JOBS_LEASE_SECONDS` TTL halten [SSOT 4.6]
- **FR-003**: Worker MUSS Lease alle `BPC_JOBS_LEASE_RENEW_SECONDS` erneuern [SSOT 4.6]
- **FR-004**: Retries MÜSSEN exponentiellen Backoff verwenden [SSOT 3.1]
- **FR-005**: Jobs MÜSSEN nach `BPC_JOBS_MAX_ATTEMPTS_DEFAULT` (Default: 10) Versuchen DEAD_LETTER werden [SSOT 4.6]
- **FR-005b**: DEAD_LETTER Jobs MÜSSEN `bpc_jobs_dead_letter_total` Metric erhöhen für Monitoring/Alerting
- **FR-006**: PARSE_FACTS MUSS Facts mit canonical Bytes und Hash speichern [SSOT 7.1]
- **FR-007**: COMPILE_PASSPORT MUSS `compilePassportPure` aus bpc-core verwenden [SSOT 3.4]
- **FR-008**: SIGN_PASSPORT MUSS ED25519 Signatur über Receipt Hash erstellen [SSOT 7.7]
- **FR-009**: GENERATE_QR MUSS BPC-QR-1 Format implementieren [SSOT 7.8]
- **FR-010**: DELIVER_WEBHOOK MUSS HMAC Signatur in X-BPC-Signature Header senden [SSOT 7.10]

### Job Types (SSOT 2.2)

| Job Type | Beschreibung | Output |
|----------|--------------|--------|
| INGEST_DOCUMENT | Document Content speichern | DocumentVersion |
| PARSE_FACTS | Facts aus Document extrahieren | Facts |
| BUILD_SNAPSHOT | Snapshot BUILDING → READY | Snapshot READY |
| COMPILE_PASSPORT | Snapshot + Rules → Passport | PassportVersion mit Artifacts |
| RUN_RULE_TESTS | Rule Tests ausführen | rule_tests_runs Row |
| SIGN_PASSPORT | Receipt signieren | signature Bytes |
| GENERATE_QR | QR PNG generieren | qr_png Bytes |
| EXPORT_PASSPORT | Passport exportieren | Export File |
| DELIVER_WEBHOOK | Webhook senden | webhook_deliveries Row |

### Exports (SSOT 3.5.4)

```haskell
module BPC.Worker.Main      -- Start worker process
module BPC.Worker.Runner    -- Job loop + leasing + metrics
module BPC.Worker.Dispatch  -- Job type → handler mapping
module BPC.Worker.Handlers.IngestDocument   -- INGEST_DOCUMENT
module BPC.Worker.Handlers.ParseFacts       -- PARSE_FACTS
module BPC.Worker.Handlers.BuildSnapshot    -- BUILD_SNAPSHOT
module BPC.Worker.Handlers.CompilePassport  -- COMPILE_PASSPORT
module BPC.Worker.Handlers.SignPassport     -- SIGN_PASSPORT
module BPC.Worker.Handlers.GenerateQR       -- GENERATE_QR
module BPC.Worker.Handlers.RunRuleTests     -- RUN_RULE_TESTS
module BPC.Worker.Handlers.ExportPassport   -- EXPORT_PASSPORT
module BPC.Worker.Handlers.DeliverWebhook   -- DELIVER_WEBHOOK
```

### Pipeline Flow (SSOT 14.8)

```
Upload DocumentVersion
    → PARSE_FACTS
    → Facts
    → Snapshot BUILDING → READY → Seal → SEALED
    → COMPILE_PASSPORT
    → SIGN_PASSPORT
    → GENERATE_QR
    → Activate → ACTIVE
```

## Success Criteria

### Measurable Outcomes

- **SC-001**: Jobs werden ohne Duplikate verarbeitet (Lock-Test mit 10 Workers)
- **SC-002**: Retry mit Backoff funktioniert für alle retryable Errors
- **SC-003**: Pipeline Upload → Active funktioniert End-to-End
- **SC-004**: COMPILE_PASSPORT verwendet deterministisches `compilePassportPure`
- **SC-005**: Webhook Signature ist mit HMAC verifizierbar
- **SC-006**: Code Coverage für bpc-worker >= 75%
