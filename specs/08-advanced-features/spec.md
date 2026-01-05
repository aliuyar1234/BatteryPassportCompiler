# Feature Specification: Advanced Features

**Feature Branch**: `08-advanced-features`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 7.9-7.11 (Algorithmen), 9.5 (Retention), 14.6 (Phase P4)
**Phase**: P4
**Packages**: bpc-api, bpc-db, bpc-worker

## User Scenarios & Testing

### User Story 1 - Policy Engine (Priority: P1)

Als Admin muss ich feinkörnige Zugriffsregeln definieren können, die über RBAC hinausgehen.

**Why this priority**: Policies ermöglichen dynamische Zugriffskontrolle ohne Code-Änderungen.

**Independent Test**: Policy erstellen die spezifische Ressourcen blockiert trotz Permission.

**Acceptance Scenarios**:

1. **Given** Actor mit `passport:read` Permission, **When** DENY Policy für spezifische passport_id existiert, **Then** wird Zugriff verweigert
2. **Given** mehrere Policies für gleiche Ressource, **When** evaluiert wird, **Then** entscheidet erste Match (nach Priority sortiert)
3. **Given** keine matching Policy, **When** Zugriff versucht wird, **Then** fällt Entscheidung auf RBAC zurück
4. **Given** Policy, **When** serialisiert wird, **Then** wird `policy_hash` aus canonical JSON berechnet

---

### User Story 2 - Token Bucket Rate Limiting (Priority: P1)

Als System muss ich API-Missbrauch durch Rate Limiting verhindern.

**Why this priority**: Rate Limiting schützt vor DoS und sichert faire Ressourcenverteilung.

**Independent Test**: Burst ausführen und 429 Response prüfen.

**Acceptance Scenarios**:

1. **Given** Rate Limit enabled, **When** Requests unter Limit, **Then** werden alle durchgelassen
2. **Given** Burst > `BPC_RATE_LIMIT_BURST`, **When** überschritten, **Then** wird 429 RATE_LIMITED mit Retry-After Header
3. **Given** Token Bucket leer, **When** Zeit vergeht, **Then** werden Tokens mit `BPC_RATE_LIMIT_RPS` refilled
4. **Given** verschiedene API Keys, **When** Rate Limits geprüft werden, **Then** sind Limits unabhängig pro Key

---

### User Story 3 - Webhook Subscriptions (Priority: P2)

Als Admin muss ich Webhook Endpoints für Events konfigurieren können.

**Why this priority**: Webhooks ermöglichen Event-driven Integrationen.

**Independent Test**: Webhook registrieren und Event-Delivery prüfen.

**Acceptance Scenarios**:

1. **Given** POST /webhook-endpoints mit URL und Secret, **When** erstellt, **Then** wird Endpoint gespeichert
2. **Given** POST /webhook-subscriptions mit endpoint_id und event_type, **When** erstellt, **Then** wird Subscription aktiv
3. **Given** aktive Subscription für `passport.version.active`, **When** Passport aktiviert wird, **Then** wird DELIVER_WEBHOOK Job erstellt
4. **Given** Webhook Delivery, **When** erfolgreich, **Then** wird Status DELIVERED gesetzt

---

### User Story 4 - Webhook HMAC Signature (Priority: P2)

Als Webhook-Empfänger muss ich die Authentizität des Payloads verifizieren können.

**Why this priority**: Ohne Signatur könnte jeder gefälschte Webhooks senden.

**Independent Test**: Webhook empfangen und Signatur verifizieren.

**Acceptance Scenarios**:

1. **Given** Webhook Delivery, **When** Request gesendet wird, **Then** enthält er `X-BPC-Signature: sha256=<hex>` Header
2. **Given** Body und Secret, **When** `hmac_sha256(secret, body)` berechnet wird, **Then** stimmt es mit Signature überein
3. **Given** manipulierter Body, **When** Signature geprüft wird, **Then** schlägt Verifikation fehl

---

### User Story 5 - HTTP Idempotency Store (Priority: P1)

Als System muss ich Idempotency Keys mit Request/Response Mapping speichern.

**Why this priority**: Idempotency ist bereits in P2 für API, aber Store-Details in P4.

**Independent Test**: Idempotency Key speichern und replayed Response prüfen.

**Acceptance Scenarios**:

1. **Given** erster Request mit Idempotency-Key, **When** verarbeitet, **Then** wird `(request_hash, response)` in `idempotency_keys` gespeichert
2. **Given** identischer Request mit gleichem Key, **When** wiederholt, **Then** wird gespeicherte Response zurückgegeben
3. **Given** anderer Request mit gleichem Key, **When** gesendet, **Then** wird 409 IDEMPOTENCY_CONFLICT

---

### User Story 6 - Data Retention (Priority: P2)

Als Admin muss ich Daten nach konfigurierbarer Zeit löschen können.

**Why this priority**: GDPR/Compliance erfordern Datenminimierung.

**Independent Test**: Retention Job ausführen und gelöschte Daten prüfen.

**Acceptance Scenarios**:

1. **Given** Events älter als `BPC_RETENTION_EVENTS_DAYS` (15 Jahre), **When** Retention Job läuft, **Then** werden sie gelöscht
2. **Given** passport_versions älter als Retention, **When** Job läuft, **Then** werden sie gelöscht
3. **Given** document_versions älter als `BPC_RETENTION_DOCUMENT_VERSIONS_DAYS` (10 Jahre), **When** Job läuft, **Then** werden sie gelöscht
4. **Given** Löschung erfolgt, **When** abgeschlossen, **Then** wird `RetentionApplied` Event mit counts+cutoff emittiert

---

### User Story 7 - Audit Events für Denied Access (Priority: P1)

Als System muss ich auch verweigerte Zugriffe protokollieren.

**Why this priority**: SSOT 0 Invariante: "Jeder denied access → Audit Event".

**Independent Test**: Zugriff ohne Permission versuchen und Audit Event prüfen.

**Acceptance Scenarios**:

1. **Given** Actor ohne Permission, **When** Zugriff verweigert wird, **Then** wird `AccessDenied` Event im Event Store erstellt
2. **Given** Policy DENY, **When** Zugriff blockiert wird, **Then** wird Event mit Policy-Details erstellt
3. **Given** Audit Event, **When** gelesen wird, **Then** enthält es `actor_id`, `resource`, `action`, `reason`

---

### User Story 8 - Policy Version Management (Priority: P2)

Als Admin muss ich Policies versionieren können ohne Breaking Changes.

**Why this priority**: Policies sind immutable wie Rules - Änderungen erfordern neue Versionen.

**Independent Test**: Policy Version erstellen und aktivieren.

**Acceptance Scenarios**:

1. **Given** Policy, **When** neue Version erstellt wird, **Then** erhält sie incrementierte Version-Nummer
2. **Given** Policy Version, **When** deaktiviert wird, **Then** wird sie bei Evaluation ignoriert
3. **Given** mehrere aktive Versions, **When** evaluiert wird, **Then** wird höchste Priority (niedrigste Zahl) zuerst geprüft

---

### Edge Cases

- Was passiert bei Rate Limit DB-Timeout? → Request wird durchgelassen (fail-open für Verfügbarkeit) [Implementation Decision]
- Was passiert bei Webhook Endpoint Timeout? → Retry mit exponentiellem Backoff, max 10 Attempts [Implementation Decision]
- Wie verhält sich Retention bei laufenden Referenzen? → ON DELETE RESTRICT verhindert Inkonsistenz [SSOT 6.2 FK constraints]
- Was ist das Policy Format? → JSON (keine eigene DSL): `{effect: "ALLOW"|"DENY", target: "...", conditions: {...}, priority: int}` [SSOT 6.4]
- Wie sieht der Webhook Retry Schedule aus? → Exponentieller Backoff: 2^n Sekunden (1s, 2s, 4s, 8s, 16s, 32s, 64s, 128s, 256s, 512s) für 10 Attempts [Implementation Decision, aligned mit FR-007b]

## Requirements

### Functional Requirements

- **FR-001**: Policy Engine MUSS first-match Evaluation mit Priority-Sortierung implementieren [SSOT 2.3]
- **FR-002**: Policy MUSS ALLOW/DENY Effect unterstützen [SSOT 6.4]
- **FR-002b**: Policy Format: JSON mit `{effect, target, conditions, priority}` Struktur (keine eigene DSL)
- **FR-003**: Policy Hash MUSS aus canonical JSON berechnet werden [SSOT 6.4]
- **FR-004**: Rate Limiting MUSS Token Bucket nach BPC-RL-1 implementieren [SSOT 7.9]
- **FR-005**: Rate Limit State MUSS in `rate_limit_buckets` Tabelle persistiert werden [SSOT 6.5]
- **FR-006**: 429 Response MUSS `Retry-After` Header enthalten [SSOT 7.9]
- **FR-007**: Webhook Signature MUSS nach BPC-WH-1 berechnet werden [SSOT 7.10]
- **FR-007b**: Webhook Retries MÜSSEN exponentiellen Backoff verwenden; Implementation: 1s, 2s, 4s, 8s, 16s, 32s, 64s, 128s, 256s, 512s (10 Attempts) - aligned mit Job Retry Pattern [nicht in SSOT, Best Practice]
- **FR-008**: Idempotency MUSS nach BPC-IDEMP-1 implementiert sein [SSOT 7.11]
- **FR-009**: Retention MUSS konfigurierbare Limits aus ENV verwenden [SSOT 4.6]
- **FR-010**: Retention MUSS `RetentionApplied` Event emittieren [SSOT 9.5]
- **FR-011**: Denied Access MUSS Audit Event generieren [SSOT 0]

### Database Tables (SSOT 6.3-6.5)

```sql
-- Policies (Migration 003)
policies (policy_id, tenant_id, name)
policy_versions (policy_version_id, tenant_id, policy_id, version,
                 is_active, effect, target, priority,
                 policy_json, policy_canonical, policy_hash)

-- Webhooks (Migration 003)
webhook_endpoints (webhook_endpoint_id, tenant_id, url, secret_base64, is_active)
webhook_subscriptions (webhook_subscription_id, tenant_id, webhook_endpoint_id, event_type)
webhook_deliveries (webhook_delivery_id, tenant_id, webhook_endpoint_id, event_id,
                    attempt, status, last_error, delivered_at)

-- Rate Limits (Migration 004)
rate_limit_buckets (tenant_id, key_hash, capacity, tokens, refill_per_second, updated_at)
```

### Webhook Event Types (SSOT 10.6)

| Event Type | Trigger |
|------------|---------|
| passport.version.signed | PassportVersion Status → SIGNED |
| passport.version.active | PassportVersion Status → ACTIVE |
| passport.version.revoked | PassportVersion Status → REVOKED |
| rules.version.published | RulePackageVersion Status → PUBLISHED |
| document.version.validated | DocumentVersion Status → VALIDATED |

### Rate Limit Algorithm (BPC-RL-1)

```
consume(key, tokens_needed):
  row = SELECT ... FOR UPDATE WHERE key_hash = hash(key)
  elapsed = now() - row.updated_at
  refilled = min(row.capacity, row.tokens + elapsed * row.refill_per_second)
  if refilled < tokens_needed:
    return DENIED with retry_after = (tokens_needed - refilled) / refill_per_second
  UPDATE tokens = refilled - tokens_needed, updated_at = now()
  return ALLOWED
```

### Retention Defaults (SSOT 4.6)

| Data Type | Default Retention |
|-----------|-------------------|
| events | 15 years (5475 days) |
| passport_versions | 15 years |
| document_versions | 10 years (3650 days) |

## Success Criteria

### Measurable Outcomes

- **SC-001**: Policy Engine blockiert/erlaubt korrekt basierend auf Priorität
- **SC-002**: Rate Limiting hält konfiguriertes RPS/Burst exakt ein
- **SC-003**: Webhook Signature ist mit Standard HMAC-SHA256 verifizierbar
- **SC-004**: Retention Job löscht nur Daten außerhalb Retention Period
- **SC-005**: Denied Access Events erscheinen im Audit Log
- **SC-006**: Alle P4-Tasks aus SSOT 14.6 sind implementiert
