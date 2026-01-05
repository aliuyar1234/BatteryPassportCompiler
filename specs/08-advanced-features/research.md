# Research: Advanced Features

**Feature**: 08-advanced-features
**Date**: 2025-12-28
**Status**: Complete

## Research Summary

Advanced features provide enterprise-grade capabilities: fine-grained access control, rate limiting, event-driven integrations, and compliance-ready data retention.

---

## 1. Policy Engine Design

**Decision**: First-match evaluation with priority ordering

**Pattern**:
- Policies have priority (lower = higher priority)
- First matching policy wins
- If no match, fall back to RBAC
- DENY overrides ALLOW at same priority

**Use Case**: Block specific passport access despite general permission
```json
{
  "effect": "DENY",
  "target": "/v1/passports/specific-id",
  "priority": 10
}
```

---

## 2. Token Bucket Algorithm

**Decision**: Database-backed token bucket per API key

**Algorithm** (BPC-RL-1):
1. SELECT ... FOR UPDATE (lock bucket)
2. Compute refilled tokens: `min(capacity, tokens + elapsed * rate)`
3. If tokens >= 1: consume and return ALLOWED
4. Else: return DENIED with retry_after

**Why DB-Backed**:
- Works across multiple workers
- Survives restarts
- Simple atomic UPDATE

---

## 3. Webhook Security

**Decision**: HMAC-SHA256 signature

**Header Format**: `X-BPC-Signature: sha256=<hex>`

**Verification by Receiver**:
```python
import hmac
import hashlib

def verify_signature(secret, body, signature_header):
    expected = hmac.new(secret, body, hashlib.sha256).hexdigest()
    actual = signature_header.replace("sha256=", "")
    return hmac.compare_digest(expected, actual)
```

---

## 4. Idempotency Strategy

**Decision**: Hash request body, store response

**Flow**:
1. Hash request body with SHA-256
2. Lookup key in idempotency_keys
3. If exists with same hash → replay response
4. If exists with different hash → 409 IDEMPOTENCY_CONFLICT
5. Otherwise → execute and store

**TTL**: 24 hours (garbage collected)

---

## 5. Data Retention

**Decision**: Scheduled job with configurable retention periods

**Defaults** (SSOT 4.6):
| Data Type | Retention |
|-----------|-----------|
| events | 15 years (5475 days) |
| passport_versions | 15 years |
| document_versions | 10 years (3650 days) |

**Deletion Order**: Must respect foreign keys (cascade)

---

## 6. Access Denied Auditing

**Decision**: Every denied access generates audit event

**Why**: SSOT 0 invariant - "Every denied access → Audit Event"

**Event Type**: `AccessDenied`

**Payload**:
```json
{
  "permission": "passport:compile",
  "reason": "RBAC",
  "correlation_id": "uuid"
}
```

---

## Open Questions

None - SSOT provides complete specifications.

## Next Steps

Proceed to implementation following plan.md phases.
