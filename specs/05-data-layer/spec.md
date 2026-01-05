# Feature Specification: Data Layer

**Feature Branch**: `05-data-layer`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 6 (Datenmodell), 3.5.2 (bpc-db Exports), 7.4 (Event Hash Chain)
**Phase**: P2
**Package**: bpc-db

## User Scenarios & Testing

### User Story 1 - Connection Pool Management (Priority: P1)

Als System muss ich einen konfigurierbaren DB Connection Pool haben für effiziente Datenbankzugriffe.

**Why this priority**: Ohne Pool keine Datenbankverbindungen - absolute Basis für alle DB-Operationen.

**Independent Test**: Pool erstellen, Connection abrufen, Query ausführen.

**Acceptance Scenarios**:

1. **Given** DB-Konfiguration aus ENV, **When** `mkPool` aufgerufen wird, **Then** wird Pool mit `BPC_DB_POOL_SIZE` Connections erstellt
2. **Given** aktiver Pool, **When** `withConn` aufgerufen wird, **Then** wird Connection ausgecheckt und nach Nutzung zurückgegeben
3. **Given** Pool unter Last, **When** alle Connections belegt sind, **Then** warten neue Requests bis Connection frei wird
4. **Given** DB nicht erreichbar, **When** Connection versucht wird, **Then** wird DB_UNAVAILABLE Error zurückgegeben

---

### User Story 2 - Event Store (Append-Only Log) (Priority: P1)

Als System muss ich ein tamper-evident Audit Log mit Hash Chain führen.

**Why this priority**: Jeder Write und jeder denied access MUSS Event generieren (SSOT 0 Invariante).

**Independent Test**: Events appenden und Hash Chain verifizieren.

**Acceptance Scenarios**:

1. **Given** neues Event, **When** `appendEvent` aufgerufen wird, **Then** wird Event mit `event_hash` und `prev_event_hash` gespeichert
2. **Given** Event mit falscher `aggregate_version`, **When** append versucht wird, **Then** wird EVENT_VERSION_CONFLICT Error zurückgegeben
3. **Given** Event Store mit Events, **When** `verifyChain` aufgerufen wird, **Then** werden alle Hash-Links verifiziert
4. **Given** manipuliertes Event (falscher Hash), **When** `verifyChain` läuft, **Then** wird Manipulation erkannt

---

### User Story 3 - Tenant Isolation (Priority: P1)

Als System muss ich garantieren, dass jeder Datenzugriff tenant-scoped ist.

**Why this priority**: Multi-Tenancy ist architekturelles Grundprinzip - alle Daten sind tenant-scoped.

**Independent Test**: Query ohne TenantId schlägt fehl; Query mit falschem TenantId gibt keine Daten zurück.

**Acceptance Scenarios**:

1. **Given** Repository-Funktion, **When** sie aufgerufen wird, **Then** MUSS TenantId als Parameter akzeptiert werden
2. **Given** Daten von Tenant A, **When** Query mit Tenant B ausgeführt wird, **Then** werden keine Daten zurückgegeben
3. **Given** Insert-Operation, **When** ausgeführt wird, **Then** wird TenantId automatisch gesetzt
4. **Given** Daten ohne TenantId, **When** Insert versucht wird, **Then** schlägt er mit Constraint-Verletzung fehl

---

### User Story 4 - Document Repository (Priority: P1)

Als System muss ich Dokumente und deren Versionen speichern und abrufen können.

**Why this priority**: Documents sind die Eingangsdaten für Facts - ohne sie keine Passport-Daten.

**Independent Test**: Document erstellen, Version uploaden, Content abrufen.

**Acceptance Scenarios**:

1. **Given** neues Document, **When** `createDocument` aufgerufen wird, **Then** wird Document mit unique ID erstellt
2. **Given** Document, **When** `uploadVersion` mit Bytes aufgerufen wird, **Then** wird DocumentVersion mit SHA-256 Hash gespeichert
3. **Given** DocumentVersion, **When** `getContent` aufgerufen wird, **Then** werden Original-Bytes zurückgegeben
4. **Given** Upload mit identischem SHA-256, **When** versucht wird, **Then** wird Duplicate-Constraint verletzt

---

### User Story 5 - Facts Repository (Priority: P1)

Als System muss ich Facts speichern mit canonical Bytes und Hash.

**Why this priority**: Facts sind die strukturierten Daten aus Documents - Basis für Snapshots.

**Independent Test**: Fact erstellen, canonical Bytes und Hash validieren.

**Acceptance Scenarios**:

1. **Given** neue Fact-Daten, **When** `insertFact` aufgerufen wird, **Then** werden `payload_canonical` und `payload_hash` berechnet und gespeichert
2. **Given** Fact mit Type/Key, **When** `getFact` aufgerufen wird, **Then** wird Fact oder Nothing zurückgegeben
3. **Given** Fact Key Prefix, **When** `getFactsByPrefix` aufgerufen wird, **Then** werden alle matching Facts zurückgegeben
4. **Given** identischer Fact (Type/Key/SchemaVersion/PayloadHash), **When** Insert versucht wird, **Then** wird Duplikat erkannt

---

### User Story 6 - Snapshots Repository (Priority: P1)

Als System muss ich Snapshots erstellen, Items hinzufügen und versiegeln können.

**Why this priority**: Snapshots bündeln Facts für Compilation - zentrale Datenstruktur.

**Independent Test**: Snapshot erstellen, Facts hinzufügen, versiegeln.

**Acceptance Scenarios**:

1. **Given** neuer Snapshot, **When** `createSnapshot` aufgerufen wird, **Then** wird Snapshot mit Status BUILDING erstellt
2. **Given** Snapshot BUILDING, **When** `addItem(factId)` aufgerufen wird, **Then** wird Fact zum Snapshot hinzugefügt
3. **Given** Snapshot READY, **When** `sealSnapshot` aufgerufen wird, **Then** wird Status SEALED, `snapshot_canonical` und `snapshot_hash` berechnet
4. **Given** Snapshot SEALED, **When** `addItem` versucht wird, **Then** wird SNAPSHOT_SEALED Error zurückgegeben

---

### User Story 7 - Passports Repository (Priority: P1)

Als System muss ich Passports und deren Versionen verwalten können.

**Why this priority**: PassportVersions sind das finale Produkt des Compilers.

**Independent Test**: Passport erstellen, Version hinzufügen, aktivieren.

**Acceptance Scenarios**:

1. **Given** neuer Passport für BatteryProduct, **When** `createPassport` aufgerufen wird, **Then** wird Passport erstellt
2. **Given** Passport, **When** `insertVersion` mit Compile-Output aufgerufen wird, **Then** wird PassportVersion mit allen Artifacts gespeichert
3. **Given** PassportVersion SIGNED, **When** `activate` aufgerufen wird, **Then** wird Status ACTIVE und vorherige Version SUPERSEDED
4. **Given** PassportVersion, **When** `revoke` aufgerufen wird, **Then** wird Status REVOKED mit Timestamp

---

### User Story 8 - Jobs Repository (Priority: P1)

Als System muss ich Jobs enqueuen, leasen und abschließen können.

**Why this priority**: Job Queue ermöglicht asynchrone Verarbeitung - kritisch für Skalierbarkeit.

**Independent Test**: Job enqueuen, leasen, erfolgreich abschließen.

**Acceptance Scenarios**:

1. **Given** neuer Job, **When** `enqueue` aufgerufen wird, **Then** wird Job mit Status QUEUED und Idempotency Key gespeichert
2. **Given** QUEUED Jobs, **When** `acquireLease` aufgerufen wird, **Then** wird ein Job mit Lease und Expiry zurückgegeben
3. **Given** geleaseter Job, **When** `complete` aufgerufen wird, **Then** wird Status SUCCEEDED
4. **Given** fehlgeschlagener Job unter max_attempts, **When** `fail` aufgerufen wird, **Then** wird Status QUEUED mit erhöhtem attempts und backoff

---

### User Story 9 - Rules Repository (Priority: P1)

Als System muss ich RulePackages und deren Versionen verwalten können.

**Why this priority**: Rules definieren die Compilation-Logik.

**Independent Test**: RulePackage erstellen, Version hinzufügen, publishen.

**Acceptance Scenarios**:

1. **Given** neues RulePackage, **When** `createPackage` aufgerufen wird, **Then** wird Package erstellt
2. **Given** Package, **When** `createVersion` mit DSL Source aufgerufen wird, **Then** wird Version mit Status DRAFT und DSL/Tests Hashes erstellt
3. **Given** Version VALIDATED mit >= 500 Testfällen, **When** `publish` aufgerufen wird, **Then** wird Status PUBLISHED
4. **Given** Version ohne Tests, **When** `publish` versucht wird, **Then** wird RULE_TESTS_FAILED Error zurückgegeben

---

### Edge Cases

- Was passiert bei DB-Timeout? → Retry mit Backoff, dann DB_UNAVAILABLE
- Was passiert bei Constraint-Verletzung? → Spezifischer Error (NOT_FOUND, CONFLICT, etc.)
- Wie verhält sich Pool bei Connection-Leak? → Timeout und Logging
- Was ist das Transaction Isolation Level? → `READ COMMITTED` (PostgreSQL Default, kein dirty read)
- Was passiert bei Pool-Erschöpfung? → Warten auf freie Connection oder Timeout
- Wie funktioniert Pagination? → Cursor-basiert mit `after_cursor` und `limit` (Default 50, Max 200) [SSOT 10.2]

## Requirements

### Functional Requirements

- **FR-001**: Pool MUSS `BPC_DB_POOL_SIZE` Connections managen (Default: 10) [SSOT 4.6]
- **FR-002**: Alle Repository-Funktionen MÜSSEN `TenantId` als ersten Parameter haben [SSOT 5.4]
- **FR-003**: Alle Transaktionen MÜSSEN mit Isolation Level `READ COMMITTED` laufen (PostgreSQL Default)
- **FR-004**: Listen-Abfragen MÜSSEN Cursor-basierte Pagination unterstützen (nach `id` sortiert, Limit 1-200, Default 50) [SSOT 10.2]
- **FR-005**: Event Store MUSS Hash Chain nach BPC-EVENT-1 implementieren [SSOT 7.4]
- **FR-006**: Event Store MUSS EVENT_VERSION_CONFLICT bei Concurrent Append zurückgeben [SSOT 7.4]
- **FR-007**: Documents MÜSSEN SHA-256 Hash des Contents speichern [SSOT 6.2]
- **FR-008**: Facts MÜSSEN `payload_canonical` und `payload_hash` speichern [SSOT 6.2]
- **FR-009**: Snapshots MÜSSEN Seal nach BPC-SNAPSHOT-1 implementieren [SSOT 7.3]
- **FR-010**: SEALED Snapshots DÜRFEN NICHT modifiziert werden [SSOT 0, 7.3, Constitution III]
- **FR-011**: Jobs MÜSSEN Idempotency Key für Deduplizierung unterstützen [SSOT 6.2]
- **FR-012**: Jobs MÜSSEN Lease mit Expiry und Renewal unterstützen [SSOT 4.6]
- **FR-013**: PassportVersion Status-Übergänge: COMPILING → SIGNED → ACTIVE → SUPERSEDED/REVOKED [SSOT 2.2]
- **FR-014**: Migration 003 MUSS policies, policy_versions, webhook_endpoints, webhook_subscriptions, webhook_deliveries Tabellen erstellen [SSOT 6.4]
- **FR-015**: Migration 004 MUSS rate_limit_buckets Tabelle erstellen [SSOT 6.5]

### Key Entities (DDL SSOT 6.2-6.5)

- **tenants**: Mandanten mit slug und name
- **actors**: Identitäten (USER/API_CLIENT/SERVICE)
- **api_keys**: API Key Hashes mit prefix
- **roles, permissions, role_permissions, actor_roles**: RBAC
- **idempotency_keys**: HTTP Idempotency Store [SSOT 6.2]
- **events**: Append-only Event Store mit Hash Chain
- **documents, document_versions**: Dokumente und Versionen
- **facts**: Strukturierte Daten mit canonical Bytes
- **data_snapshots, snapshot_items**: Fact-Bundles
- **battery_products**: Produkte
- **passports, passport_versions**: Finale Passport-Daten
- **rule_packages, rule_package_versions, rule_fields, rule_tests_runs**: Rule-Verwaltung
- **jobs**: Job Queue
- **policies, policy_versions**: Policy Engine [SSOT 6.4]
- **webhook_endpoints, webhook_subscriptions, webhook_deliveries**: Webhooks [SSOT 6.4]
- **rate_limit_buckets**: Token Bucket Rate Limiting [SSOT 6.5]

### Exports (SSOT 3.5.2)

```haskell
-- Pool
mkPool :: IO Pool
withConn :: Pool -> (Connection -> IO a) -> IO a

-- Repos
module BPC.DB.Repos.Events     -- appendEvent, verifyChain
module BPC.DB.Repos.Auth       -- Actors/ApiKeys/Roles/Permissions/Policies
module BPC.DB.Repos.Documents  -- Documents + DocumentVersions
module BPC.DB.Repos.Facts      -- Facts insert/query
module BPC.DB.Repos.Snapshots  -- Snapshots + Items + Seal
module BPC.DB.Repos.Rules      -- RulePackages + Versions + Tests
module BPC.DB.Repos.Passports  -- Passports + PassportVersions
module BPC.DB.Repos.Jobs       -- enqueue, acquireLease, complete/fail
module BPC.DB.Repos.Webhooks   -- webhook endpoints/subscriptions/deliveries [SSOT 3.5.2]
module BPC.DB.Repos.RateLimit  -- rate_limit_buckets [SSOT 6.5]
```

## Success Criteria

### Measurable Outcomes

- **SC-001**: Alle Repository-Funktionen akzeptieren TenantId als ersten Parameter
- **SC-002**: Event Chain Verification funktioniert für 10.000+ Events
- **SC-003**: Snapshot Seal ist deterministisch (gleiche Facts → gleicher Hash)
- **SC-004**: Job Leasing ist race-condition-frei (kein Double-Run)
- **SC-005**: Integration Tests mit docker-compose Postgres passieren
- **SC-006**: Code Coverage für bpc-db >= 80%
