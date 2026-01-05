# Quickstart: API Server

**Feature**: 06-api-server
**Date**: 2025-12-28
**Package**: bpc-api

## Prerequisites

- 05-data-layer complete (repositories working)
- Database running with migrations applied

## Running the Server

```bash
# Set environment
export BPC_DB_HOST=localhost
export BPC_DB_PORT=5432
export BPC_DB_USER=bpc
export BPC_DB_PASSWORD=bpc
export BPC_DB_NAME=bpc
export BPC_HTTP_PORT=8080
export BPC_API_KEY_PEPPER_BASE64=$(echo -n "dev-pepper" | base64)

# Run server
cabal run bpc-api:exe:bpc-api
```

## Quick API Usage

### Health Check

```bash
# Liveness (no auth required)
curl http://localhost:8080/v1/health/live
# {"status": "OK"}

# Readiness (no auth required)
curl http://localhost:8080/v1/health/ready
# {"status": "OK", "checks": {"database": {"status": "OK"}}}
```

### Authenticated Request

```bash
# Get API key from seed data
API_KEY="bpc_dev_xxxxx"

# List documents
curl -H "Authorization: Bearer $API_KEY" \
     http://localhost:8080/v1/documents
# {"items": [], "next_cursor": null}
```

### Create Document

```bash
# Create with idempotency key
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Content-Type: application/json" \
     -H "Idempotency-Key: doc-001" \
     -d '{"kind": "BOM", "external_ref": "bom-001"}' \
     http://localhost:8080/v1/documents
# {"document_id": "uuid"}
```

### Upload Document Version

```bash
# Base64 encode content
CONTENT=$(echo '{"parts": []}' | base64)

curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Content-Type: application/json" \
     -H "Idempotency-Key: version-001" \
     -d "{\"content\": \"$CONTENT\", \"mime_type\": \"application/json\"}" \
     "http://localhost:8080/v1/documents/$DOC_ID/versions"
# {"document_version_id": "uuid", "sha256": "..."}
```

### Create and Seal Snapshot

```bash
# Create snapshot
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: snap-001" \
     -d '{"label": "Q1 2025"}' \
     http://localhost:8080/v1/snapshots
# {"snapshot_id": "uuid", "status": "BUILDING"}

# Add items
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: item-001" \
     -d "{\"fact_id\": \"$FACT_ID\"}" \
     "http://localhost:8080/v1/snapshots/$SNAP_ID/items"

# Seal
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: seal-001" \
     "http://localhost:8080/v1/snapshots/$SNAP_ID/seal"
# {"snapshot_id": "uuid", "snapshot_hash": "...", "sealed_at": "..."}
```

### Trigger Compilation

```bash
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: compile-001" \
     -d "{\"snapshot_id\": \"$SNAP_ID\", \"rule_package_version_id\": \"$RULES_ID\"}" \
     "http://localhost:8080/v1/passports/$PASSPORT_ID/compile"
# {"passport_version_id": "uuid", "job_id": "uuid", "status": "ACCEPTED"}
```

### Get Passport Artifacts

```bash
# Metadata
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/passport-versions/$PV_ID"

# Payload JSON
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/passport-versions/$PV_ID/payload"

# Proof JSON
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/passport-versions/$PV_ID/proof"

# Receipt JSON
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/passport-versions/$PV_ID/receipt"

# QR PNG
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/passport-versions/$PV_ID/qr.png" > qr.png
```

### Replay/Verify

```bash
curl -X POST \
     -H "Authorization: Bearer $API_KEY" \
     -H "Idempotency-Key: replay-001" \
     "http://localhost:8080/v1/passport-versions/$PV_ID/replay"
# {"verification": "passed"}
```

### Pagination

```bash
# First page
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/documents?limit=10"
# {"items": [...], "next_cursor": "base64..."}

# Next page
curl -H "Authorization: Bearer $API_KEY" \
     "http://localhost:8080/v1/documents?limit=10&cursor=base64..."
```

## Error Responses

```bash
# Missing auth
curl http://localhost:8080/v1/documents
# 401 {"error": {"code": "UNAUTHORIZED", ...}}

# Missing permission
curl -H "Authorization: Bearer $AUDITOR_KEY" \
     -X POST http://localhost:8080/v1/documents ...
# 403 {"error": {"code": "FORBIDDEN", ...}}

# Idempotency conflict
curl -H "Idempotency-Key: same-key" -d '{"different": "body"}' ...
# 409 {"error": {"code": "IDEMPOTENCY_CONFLICT", ...}}

# Rate limited
# 429 {"error": {"code": "RATE_LIMITED", "details": {"retry_after": 5}, ...}}
```

## Testing

```bash
# Run API tests
cabal test bpc-api

# Run with coverage
cabal test bpc-api --enable-coverage
```

## Next Steps

After API server is complete:

1. **07-job-processing**: Handles async compile/sign/qr jobs
2. **08-advanced-features**: Rate limiting, webhooks, policies
