# Quickstart: Advanced Features

**Feature**: 08-advanced-features
**Date**: 2025-12-28
**Packages**: bpc-api, bpc-db, bpc-worker

## Prerequisites

- 06-api-server and 07-job-processing complete
- Migrations 003 and 004 applied

## Policy Engine

### Create a Policy

```bash
# Create DENY policy for specific resource
curl -X POST \
     -H "Authorization: Bearer $ADMIN_KEY" \
     -H "Idempotency-Key: policy-001" \
     -d '{
       "name": "Block Legacy Passport",
       "effect": "DENY",
       "target": "/v1/passports/legacy-id",
       "priority": 10
     }' \
     http://localhost:8080/v1/policies
```

### Test Policy Effect

```bash
# This should be denied even with passport:read permission
curl -H "Authorization: Bearer $API_KEY" \
     http://localhost:8080/v1/passports/legacy-id
# 403 {"error": {"code": "FORBIDDEN", "message": "Blocked by policy: Block Legacy Passport"}}
```

## Rate Limiting

### Enable Rate Limiting

```bash
export BPC_RATE_LIMIT_ENABLED=true
export BPC_RATE_LIMIT_RPS=50    # 50 tokens/second refill
export BPC_RATE_LIMIT_BURST=100 # 100 token capacity
```

### Test Rate Limit

```bash
# Rapid requests will eventually hit limit
for i in {1..200}; do
  curl -s -o /dev/null -w "%{http_code}\n" \
       -H "Authorization: Bearer $API_KEY" \
       http://localhost:8080/v1/documents
done

# After ~100 requests: 429 with Retry-After header
```

## Webhooks

### Create Webhook Endpoint

```bash
# Create endpoint with secret
SECRET=$(openssl rand -base64 32)
curl -X POST \
     -H "Authorization: Bearer $ADMIN_KEY" \
     -H "Idempotency-Key: webhook-001" \
     -d "{
       \"url\": \"https://my-service.example.com/webhooks/bpc\",
       \"secret\": \"$SECRET\"
     }" \
     http://localhost:8080/v1/webhook-endpoints
```

### Subscribe to Events

```bash
# Subscribe to passport activation events
curl -X POST \
     -H "Authorization: Bearer $ADMIN_KEY" \
     -H "Idempotency-Key: sub-001" \
     -d "{
       \"endpoint_id\": \"$ENDPOINT_ID\",
       \"event_type\": \"passport.version.active\"
     }" \
     http://localhost:8080/v1/webhook-subscriptions
```

### Verify Webhook Signature (Receiver)

```python
import hmac
import hashlib

def verify_bpc_webhook(secret, body, signature_header):
    """Verify BPC webhook signature"""
    expected_sig = hmac.new(
        secret.encode(),
        body,
        hashlib.sha256
    ).hexdigest()

    actual_sig = signature_header.replace("sha256=", "")
    return hmac.compare_digest(expected_sig, actual_sig)

# Flask example
@app.route('/webhooks/bpc', methods=['POST'])
def handle_bpc_webhook():
    signature = request.headers.get('X-BPC-Signature')
    if not verify_bpc_webhook(SECRET, request.data, signature):
        return 'Invalid signature', 401

    event = request.json
    # Process event...
    return 'OK', 200
```

## Idempotency

### First Request

```bash
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: create-doc-123" \
     -d '{"kind": "BOM"}' \
     http://localhost:8080/v1/documents
# 201 {"document_id": "uuid"}
```

### Replay Same Request

```bash
# Same request, same key → same response
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: create-doc-123" \
     -d '{"kind": "BOM"}' \
     http://localhost:8080/v1/documents
# 201 {"document_id": "uuid"}  # Same response!
```

### Conflict on Different Request

```bash
# Different request, same key → conflict
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: create-doc-123" \
     -d '{"kind": "PCF"}' \
     http://localhost:8080/v1/documents
# 409 {"error": {"code": "IDEMPOTENCY_CONFLICT"}}
```

## Data Retention

### Configure Retention

```bash
export BPC_RETENTION_EVENTS_DAYS=5475              # 15 years
export BPC_RETENTION_PASSPORT_VERSIONS_DAYS=5475   # 15 years
export BPC_RETENTION_DOCUMENT_VERSIONS_DAYS=3650   # 10 years
```

### Run Retention Job

```bash
# Manual run (usually scheduled via cron/k8s CronJob)
cabal run bpc-cli -- retention --dry-run  # Preview
cabal run bpc-cli -- retention            # Execute
```

### Check Retention Event

```sql
SELECT payload FROM events
WHERE event_type = 'RetentionApplied'
ORDER BY occurred_at DESC
LIMIT 1;
-- {"events_deleted": 1234, "passport_versions_deleted": 0, ...}
```

## Audit Denied Access

### View Access Denied Events

```bash
# List denied access events
curl -H "Authorization: Bearer $ADMIN_KEY" \
     "http://localhost:8080/v1/audit/events?event_type=AccessDenied"
```

### SQL Query

```sql
SELECT
  occurred_at,
  actor_id,
  payload->>'permission' as permission,
  payload->>'reason' as reason
FROM events
WHERE tenant_id = 'tenant-uuid'
  AND event_type = 'AccessDenied'
ORDER BY occurred_at DESC;
```

## Testing

```bash
# Run all advanced feature tests
cabal test bpc-api --test-option="--pattern=Advanced"
cabal test bpc-worker --test-option="--pattern=Webhook"
cabal test bpc-worker --test-option="--pattern=Retention"
```
