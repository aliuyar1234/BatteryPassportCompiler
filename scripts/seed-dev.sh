#!/usr/bin/env bash
# BPC Development Seed Script
# Creates test actor and API key for local development

set -euo pipefail

# Database connection
DB_HOST="${BPC_DB_HOST:-localhost}"
DB_PORT="${BPC_DB_PORT:-5432}"
DB_USER="${BPC_DB_USER:-bpc}"
DB_NAME="${BPC_DB_NAME:-bpc}"
export PGPASSWORD="${BPC_DB_PASSWORD:-bpc}"

echo "Seeding development data..."
echo "Database: ${DB_HOST}:${DB_PORT}/${DB_NAME}"

# Dev tenant ID (from migration 002)
DEV_TENANT_ID="00000000-0000-0000-0000-000000000001"

# Test actor IDs
DEV_ACTOR_ID="00000000-0000-0000-0000-000000000201"
DEV_API_KEY_ID="00000000-0000-0000-0000-000000000301"

# Admin role ID (from migration 002)
ADMIN_ROLE_ID="00000000-0000-0000-0000-000000000101"

# API key: bpc_dev_key_1234567890 (prefix: bpc_dev_)
# Hash: SHA-256 of key (in real system, this would include pepper)
# For dev, we use a placeholder hash
DEV_KEY_HASH="\\x$(echo -n 'bpc_dev_key_1234567890' | sha256sum | cut -d' ' -f1)"

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" <<EOF
-- Insert dev actor (idempotent)
INSERT INTO actors (actor_id, tenant_id, type, display_name, email, is_active)
VALUES (
  '${DEV_ACTOR_ID}',
  '${DEV_TENANT_ID}',
  'API_CLIENT',
  'Dev Test Actor',
  'dev@example.com',
  true
)
ON CONFLICT (actor_id) DO NOTHING;

-- Insert dev API key (idempotent)
INSERT INTO api_keys (api_key_id, tenant_id, actor_id, key_prefix, key_hash, label)
VALUES (
  '${DEV_API_KEY_ID}',
  '${DEV_TENANT_ID}',
  '${DEV_ACTOR_ID}',
  'bpc_dev_',
  ${DEV_KEY_HASH},
  'Development API Key'
)
ON CONFLICT (api_key_id) DO NOTHING;

-- Grant Admin role to dev actor (idempotent)
INSERT INTO actor_roles (tenant_id, actor_id, role_id)
VALUES (
  '${DEV_TENANT_ID}',
  '${DEV_ACTOR_ID}',
  '${ADMIN_ROLE_ID}'
)
ON CONFLICT (tenant_id, actor_id, role_id) DO NOTHING;
EOF

echo ""
echo "Development data seeded successfully!"
echo ""
echo "Test API Key: bpc_dev_key_1234567890"
echo "Test Actor ID: ${DEV_ACTOR_ID}"
echo "Test Tenant ID: ${DEV_TENANT_ID}"
echo ""
echo "Use this API key for local development testing."
