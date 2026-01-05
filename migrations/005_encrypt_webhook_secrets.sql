-- Migration 005: Encrypted Webhook Secrets
-- Battery Passport Compiler - Security Enhancement
--
-- This migration adds support for encrypted webhook secrets.
-- The application encrypts secrets before storing and decrypts on read.

-- Add encrypted secret column
ALTER TABLE webhook_endpoints
ADD COLUMN encrypted_secret TEXT;

-- Add flag to indicate if secret is encrypted
ALTER TABLE webhook_endpoints
ADD COLUMN secret_encrypted BOOLEAN NOT NULL DEFAULT FALSE;

-- Index for migration status tracking
CREATE INDEX idx_webhook_endpoints_encryption
ON webhook_endpoints (secret_encrypted)
WHERE secret_encrypted = FALSE;

-- Comment explaining the encryption scheme
COMMENT ON COLUMN webhook_endpoints.encrypted_secret IS
  'AES-256-CTR encrypted secret with HMAC-SHA256 authentication. Format: base64(iv || ciphertext || hmac)';

COMMENT ON COLUMN webhook_endpoints.secret_encrypted IS
  'TRUE if secret is stored encrypted, FALSE for legacy plaintext secrets pending migration';

-- Note: Existing secrets will be migrated via application-level batch job
-- that reads plaintext, encrypts, and updates encrypted_secret + secret_encrypted
