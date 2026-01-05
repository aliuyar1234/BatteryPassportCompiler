# Research: API Server

**Feature**: 06-api-server
**Date**: 2025-12-28
**Status**: Complete

## Research Summary

API server provides HTTP interface following REST patterns. All design decisions align with SSOT Section 10.

---

## 1. HTTP Framework

**Decision**: WAI/Warp with Servant

**Rationale**:
- Warp is battle-tested, high-performance
- Servant provides type-safe routing
- WAI middleware ecosystem (logging, compression)

**Alternative**: Scotty (simpler but less type-safe)

---

## 2. Authentication Pattern

**Decision**: Bearer token with SHA-256(key+pepper)

**Flow**:
1. Extract `Authorization: Bearer <key>` header
2. Compute `SHA-256(key + pepper)`
3. Lookup hash in api_keys table
4. Verify not revoked
5. Load actor and permissions

**Security**:
- Never store plaintext keys
- Pepper from ENV (BPC_API_KEY_PEPPER_BASE64)
- Key prefix for identification

---

## 3. Cursor Pagination

**Decision**: Keyset pagination with Base64 cursor

**Rationale**:
- Stable under inserts (unlike offset)
- Efficient for large datasets
- Opaque cursor hides implementation

**Cursor Format**:
```json
{"t": "2025-01-01T00:00:00Z", "id": "uuid"}
```

**Query Pattern**:
```sql
SELECT * FROM documents
WHERE tenant_id = ?
  AND (created_at, document_id) > (cursor_time, cursor_id)
ORDER BY created_at, document_id
LIMIT ?
```

---

## 4. Idempotency

**Decision**: Request hash comparison

**Pattern**:
1. Extract `Idempotency-Key` header
2. Compute hash of request body
3. If key exists with same hash → replay response
4. If key exists with different hash → 409 IDEMPOTENCY_CONFLICT
5. Otherwise → execute and store

**TTL**: Keys expire after 24 hours (garbage collected)

---

## 5. Rate Limiting

**Decision**: Token bucket per API key (DB-backed)

**Algorithm** (BPC-RL-1):
```text
consume(key, tokens_needed):
  row = SELECT ... FOR UPDATE WHERE key_hash = hash(key)
  elapsed = now() - row.updated_at
  refilled = min(capacity, tokens + elapsed * refill_per_second)
  if refilled < tokens_needed:
    return DENIED with retry_after = (tokens_needed - refilled) / refill_per_second
  UPDATE tokens = refilled - tokens_needed, updated_at = now()
  return ALLOWED
```

**429 Response**:
```json
{
  "error": {
    "code": "RATE_LIMITED",
    "message": "Rate limit exceeded",
    "correlation_id": "...",
    "details": {"retry_after": 5}
  }
}
```

---

## 6. Error Handling

**Decision**: Structured ErrorEnvelope

**Format**:
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "correlation_id": "uuid",
    "details": {
      "field": "email",
      "reason": "invalid format"
    }
  }
}
```

**5xx Errors**: Never expose internal details.

---

## 7. GraphQL (MVP)

**Decision**: Read-only passport queries

**Scope**:
- Query passports by ID
- Query passport versions
- No mutations (use REST)

**Library**: graphql-api or morpheus-graphql

---

## Open Questions

None - SSOT specifies all patterns.

## Next Steps

Proceed to implementation following plan.md phases.
