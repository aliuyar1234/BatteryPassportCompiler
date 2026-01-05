# Feature Specification: API Server

**Feature Branch**: `06-api-server`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 10 (API), 3.5.3 (bpc-api Exports), 11 (Observability)
**Phase**: P2-P3
**Package**: bpc-api

## User Scenarios & Testing

### User Story 1 - API Key Authentication (Priority: P1)

Als API-Client muss ich mich mit einem API Key authentifizieren können.

**Why this priority**: Ohne Auth kein Zugriff auf geschützte Endpoints - absolute Basis.

**Independent Test**: Request mit gültigem/ungültigem API Key testen.

**Acceptance Scenarios**:

1. **Given** Request ohne Authorization Header, **When** geschützter Endpoint aufgerufen wird, **Then** wird 401 UNAUTHORIZED zurückgegeben
2. **Given** Request mit ungültigem API Key, **When** Endpoint aufgerufen wird, **Then** wird 401 UNAUTHORIZED zurückgegeben
3. **Given** Request mit widerrufenen API Key, **When** Endpoint aufgerufen wird, **Then** wird 401 API_KEY_REVOKED zurückgegeben
4. **Given** Request mit gültigem API Key, **When** Endpoint aufgerufen wird, **Then** wird AuthContext mit Actor und Tenant erstellt

---

### User Story 2 - RBAC Authorization (Priority: P1)

Als System muss ich Zugriff basierend auf Rollen und Permissions prüfen.

**Why this priority**: Feingranulare Zugriffskontrolle ist Sicherheitsanforderung.

**Independent Test**: Request mit/ohne benötigte Permission testen.

**Acceptance Scenarios**:

1. **Given** Actor ohne `passport:read` Permission, **When** GET /passports aufgerufen wird, **Then** wird 403 FORBIDDEN zurückgegeben
2. **Given** Actor mit `passport:read` Permission, **When** GET /passports aufgerufen wird, **Then** wird 200 mit Daten zurückgegeben
3. **Given** Actor mit Admin Rolle, **When** beliebiger Endpoint aufgerufen wird, **Then** hat er alle Permissions
4. **Given** Auditor Rolle, **When** POST /passports/compile aufgerufen wird, **Then** wird 403 FORBIDDEN (keine compile Permission)

---

### User Story 3 - Correlation ID Tracking (Priority: P1)

Als Operator muss ich Requests über alle Logs hinweg verfolgen können.

**Why this priority**: Debugging und Monitoring erfordern durchgängige Request-Korrelation.

**Independent Test**: Request senden und Correlation ID in Response und Logs prüfen.

**Acceptance Scenarios**:

1. **Given** Request ohne X-Correlation-Id, **When** verarbeitet wird, **Then** wird neue UUID generiert und im Response Header zurückgegeben
2. **Given** Request mit X-Correlation-Id, **When** verarbeitet wird, **Then** wird gleiche ID im Response Header und allen Logs verwendet
3. **Given** Error Response, **When** generiert wird, **Then** enthält ErrorEnvelope die correlation_id

---

### User Story 4 - Cursor Pagination (Priority: P1)

Als API-Client muss ich große Datenmengen seitenweise abrufen können.

**Why this priority**: Ohne Pagination würden große Responses Timeouts und Memory-Probleme verursachen.

**Independent Test**: Große Datenmenge paginiert abrufen.

**Acceptance Scenarios**:

1. **Given** 100 Passports, **When** GET /passports?limit=10 aufgerufen wird, **Then** werden 10 Items und next_cursor zurückgegeben
2. **Given** next_cursor, **When** GET /passports?cursor=<cursor> aufgerufen wird, **Then** werden die nächsten Items zurückgegeben
3. **Given** letzte Seite, **When** abgerufen wird, **Then** ist next_cursor null
4. **Given** limit > 200, **When** Request gesendet wird, **Then** wird auf 200 begrenzt

---

### User Story 5 - Idempotency (Priority: P1)

Als API-Client muss ich Requests sicher wiederholen können ohne Duplikate zu erzeugen.

**Why this priority**: Netzwerk-Retries dürfen keine Duplikate erzeugen.

**Independent Test**: Gleichen Request mit Idempotency-Key zweimal senden.

**Acceptance Scenarios**:

1. **Given** POST mit Idempotency-Key, **When** erstmals gesendet wird, **Then** wird Request ausgeführt und Response gespeichert
2. **Given** identischer Request mit gleichem Key, **When** erneut gesendet wird, **Then** wird gespeicherte Response zurückgegeben
3. **Given** anderer Request mit gleichem Key, **When** gesendet wird, **Then** wird 409 IDEMPOTENCY_CONFLICT zurückgegeben
4. **Given** mutierender Request (POST/PUT/PATCH/DELETE) ohne Idempotency-Key auf Endpoint der "Required" markiert ist [SSOT 10.4], **When** gesendet wird, **Then** wird 422 VALIDATION_ERROR zurückgegeben (Note: /v1/graphql hat "Optional")

---

### User Story 6 - Error Envelope Format (Priority: P1)

Als API-Client muss ich strukturierte Error Responses erhalten.

**Why this priority**: Konsistentes Error-Format ermöglicht zuverlässige Client-Implementierung.

**Independent Test**: Verschiedene Fehler triggern und Response-Format prüfen.

**Acceptance Scenarios**:

1. **Given** beliebiger Error, **When** Response generiert wird, **Then** enthält sie `{error: {code, message, correlation_id}}`
2. **Given** Validation Error, **When** Response generiert wird, **Then** enthält `details` Objekt mit Feldfehlern
3. **Given** 5xx Error, **When** Response generiert wird, **Then** wird keine interne Information exponiert

---

### User Story 7 - Document Endpoints (Priority: P1)

Als API-Client muss ich Dokumente hochladen und abrufen können.

**Why this priority**: Documents sind die Eingangsdaten für den gesamten Pipeline.

**Independent Test**: Document erstellen, Version hochladen, Content abrufen.

**Acceptance Scenarios**:

1. **Given** POST /documents mit Kind, **When** gesendet wird, **Then** wird 201 mit document_id zurückgegeben
2. **Given** POST /documents/{id}/versions mit Base64 Content, **When** gesendet wird, **Then** wird 201 mit document_version_id und sha256 zurückgegeben
3. **Given** GET /document-versions/{id}/content, **When** abgerufen wird, **Then** werden Original-Bytes zurückgegeben
4. **Given** nicht existierende ID, **When** abgerufen wird, **Then** wird 404 NOT_FOUND zurückgegeben

---

### User Story 8 - Snapshot Endpoints (Priority: P1)

Als API-Client muss ich Snapshots erstellen, befüllen und versiegeln können.

**Why this priority**: Snapshots bündeln Facts für Compilation.

**Independent Test**: Snapshot-Lifecycle durchlaufen.

**Acceptance Scenarios**:

1. **Given** POST /snapshots, **When** gesendet wird, **Then** wird 201 mit snapshot_id und status BUILDING zurückgegeben
2. **Given** POST /snapshots/{id}/items mit fact_id, **When** gesendet wird, **Then** wird Fact hinzugefügt
3. **Given** POST /snapshots/{id}/seal, **When** Status READY ist, **Then** wird Snapshot versiegelt mit snapshot_hash
4. **Given** Snapshot SEALED, **When** POST /items versucht wird, **Then** wird 409 SNAPSHOT_SEALED zurückgegeben

---

### User Story 9 - Passport Endpoints (Priority: P1)

Als API-Client muss ich Passports kompilieren, aktivieren und abrufen können.

**Why this priority**: Passports sind das finale Produkt.

**Independent Test**: Passport erstellen, compilieren, aktivieren.

**Acceptance Scenarios**:

1. **Given** POST /passports mit battery_product_id, **When** gesendet wird, **Then** wird 201 mit passport_id zurückgegeben
2. **Given** POST /passports/{id}/compile mit snapshot_id und rule_package_version_id, **When** gesendet wird, **Then** wird 202 Accepted (Job enqueued)
3. **Given** GET /passport-versions/{id}/payload, **When** abgerufen wird, **Then** wird canonical JSON Payload zurückgegeben
4. **Given** POST /passport-versions/{id}/activate, **When** Status SIGNED ist, **Then** wird 200 und Status ACTIVE

---

### User Story 10 - Replay Endpoint (Priority: P2)

Als Auditor muss ich eine PassportVersion verifizieren können.

**Why this priority**: Replay ist Audit-Feature für Compliance-Nachweis.

**Independent Test**: PassportVersion replayed und Hashes verglichen.

**Acceptance Scenarios**:

1. **Given** POST /passport-versions/{id}/replay, **When** Version gültig ist, **Then** wird 200 mit verification=passed zurückgegeben
2. **Given** manipulierte Version (falsche Hashes), **When** Replay ausgeführt wird, **Then** wird 409 REPLAY_MISMATCH zurückgegeben
3. **Given** Version mit Status COMPILING, **When** Replay versucht wird, **Then** wird Fehler zurückgegeben

---

### User Story 11 - Health & Metrics (Priority: P2)

Als Operator muss ich Liveness, Readiness und Metrics abrufen können.

**Why this priority**: Kubernetes Health Checks und Monitoring-Integration.

**Independent Test**: Health Endpoints aufrufen.

**Acceptance Scenarios**:

1. **Given** GET /v1/health/live, **When** Server läuft, **Then** wird 200 `{status: "OK"}` zurückgegeben
2. **Given** GET /v1/health/ready, **When** DB erreichbar, **Then** wird 200 zurückgegeben
3. **Given** GET /v1/health/ready, **When** DB nicht erreichbar, **Then** wird 503 mit ErrorEnvelope zurückgegeben
4. **Given** GET /v1/metrics, **When** abgerufen wird, **Then** wird Prometheus-Format Text zurückgegeben

---

### User Story 12 - GraphQL (MVP) (Priority: P3)

Als API-Client muss ich Daten per GraphQL abfragen können.

**Why this priority**: GraphQL ist optionale Alternative zu REST.

**Independent Test**: GraphQL Query für Passports ausführen.

**Acceptance Scenarios**:

1. **Given** POST /v1/graphql mit Query, **When** ausgeführt wird, **Then** werden angeforderte Felder zurückgegeben
2. **Given** Query mit fehlender Permission, **When** ausgeführt wird, **Then** werden nur erlaubte Felder zurückgegeben

---

### Edge Cases

- Was passiert bei sehr großem Request Body? → 413 Payload Too Large
- Was passiert bei Timeout während DB-Zugriff? → 503 mit Retry-After
- Wie verhält sich Pagination bei zwischenzeitlichen Inserts? → Cursor ist stabil, neue Items erscheinen in späteren Seiten
- Was passiert bei GraphQL Query Depth > 10? → 400 mit `GRAPHQL_DEPTH_EXCEEDED` Error
- Welche OpenAPI Version? → OpenAPI 3.0.3 (per SSOT 10.5)

## Requirements

### Functional Requirements

- **FR-001**: API MUSS REST Prefix `/v1` verwenden [SSOT 10.1]
- **FR-002**: Auth MUSS API Keys mit SHA-256(key+pepper) validieren [SSOT 2.3]
- **FR-003**: Auth MUSS RBAC mit Permissions Matrix [SSOT 14.7] implementieren
- **FR-004**: Pagination MUSS Cursor-basiert sein mit limit 1-200 [SSOT 10.2]
- **FR-005**: Mutierende Endpoints (POST/PUT/PATCH/DELETE) MÜSSEN Idempotency-Key Header erfordern wo "Required" in SSOT 10.4 (Exception: /v1/graphql ist "Optional") [SSOT 10.3]
- **FR-006**: Error Response MUSS ErrorEnvelope Schema verwenden [SSOT 10.5]
- **FR-007**: Alle Endpoints MÜSSEN X-Correlation-Id setzen [SSOT 11.1]
- **FR-008**: Alle Endpoints aus SSOT 10.4 Tabelle MÜSSEN implementiert sein
- **FR-009**: OpenAPI Spec MUSS unter /docs/openapi.yaml verfügbar sein (Version: OpenAPI 3.0.3) [SSOT 10.5]
- **FR-010**: Health Endpoints MÜSSEN ohne Auth aufrufbar sein [SSOT 11.3]
- **FR-011**: Metrics MÜSSEN Prometheus-Format liefern [SSOT 11.2]
- **FR-012**: GraphQL MUSS Query Depth Limit erzwingen (Implementation: 10, nicht in SSOT - DOS-Schutz Best Practice)
- **FR-013**: ErrorEnvelope Schema wird in `BPC.Core.Types` definiert (shared zwischen bpc-api und bpc-worker) [SSOT 10.5]

### Key Endpoints (SSOT 10.4)

| Method | Path | Permission |
|--------|------|------------|
| GET | /v1/health/live | — |
| GET | /v1/health/ready | — |
| GET | /v1/metrics | — |
| POST | /v1/documents | docs:upload |
| GET | /v1/documents | docs:read |
| POST | /v1/snapshots | snapshot:build |
| POST | /v1/snapshots/{id}/seal | snapshot:seal |
| POST | /v1/passports | passport:compile |
| POST | /v1/passports/{id}/compile | passport:compile |
| POST | /v1/passport-versions/{id}/activate | passport:compile |
| POST | /v1/passport-versions/{id}/replay | passport:replay |
| GET | /v1/audit/events | audit:read |

### Exports (SSOT 3.5.3)

```haskell
module BPC.API.Main           -- WAI/Warp server start
module BPC.API.Routes         -- Route table
module BPC.API.Middleware.Auth         -- AuthN/AuthZ
module BPC.API.Middleware.RateLimit    -- Token bucket
module BPC.API.Middleware.CorrelationId
module BPC.API.Handlers.*     -- Thin handlers
module BPC.API.GraphQL        -- MVP GraphQL
module BPC.API.OpenAPI        -- /docs/openapi.yaml
```

### Metrics (SSOT 11.2)

| Metric | Type |
|--------|------|
| bpc_http_requests_total | counter |
| bpc_http_request_duration_seconds | histogram |
| bpc_db_pool_in_use | gauge |
| bpc_rate_limited_total | counter |

## Success Criteria

### Measurable Outcomes

- **SC-001**: Alle 30+ Endpoints aus SSOT 10.4 sind implementiert
- **SC-002**: Auth/RBAC Tests decken alle 16 Permissions ab
- **SC-003**: Idempotency funktioniert für alle mutierenden Endpoints
- **SC-004**: Response Times < 100ms für einfache GET Requests (P95)
- **SC-005**: OpenAPI Spec ist valide und vollständig
- **SC-006**: Code Coverage für bpc-api >= 70%
