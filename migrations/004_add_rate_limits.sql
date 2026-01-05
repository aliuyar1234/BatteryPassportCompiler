-- Migration 004: Rate Limiting
-- Battery Passport Compiler - Token Bucket Rate Limiting (BPC-RL-1)

-- Rate limit buckets table (token bucket algorithm)
CREATE TABLE rate_limit_buckets (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    key_hash TEXT NOT NULL,
    tokens DOUBLE PRECISION NOT NULL,
    capacity DOUBLE PRECISION NOT NULL DEFAULT 100,
    refill_per_second DOUBLE PRECISION NOT NULL DEFAULT 10,
    last_refill_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT rate_limit_buckets_unique UNIQUE (tenant_id, key_hash)
);

-- Idempotency keys table (BPC-IDEMP-1)
CREATE TABLE idempotency_keys (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    key_hash TEXT NOT NULL,
    request_hash TEXT NOT NULL,
    response_status INTEGER NOT NULL,
    response_body BYTEA NOT NULL,
    response_headers JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    expires_at TIMESTAMPTZ NOT NULL DEFAULT (NOW() + INTERVAL '24 hours'),
    CONSTRAINT idempotency_keys_unique UNIQUE (tenant_id, key_hash)
);

-- Index for rate limit key lookup
CREATE INDEX idx_rate_limit_buckets_key ON rate_limit_buckets (tenant_id, key_hash);

-- Index for idempotency key lookup and cleanup
CREATE INDEX idx_idempotency_keys_lookup ON idempotency_keys (tenant_id, key_hash);
CREATE INDEX idx_idempotency_keys_expires ON idempotency_keys (expires_at);
