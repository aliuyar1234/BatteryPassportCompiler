#!/usr/bin/env bash
# BPC Migration Script
# Runs database migrations using dbmate

set -euo pipefail

# Set DATABASE_URL from environment or build from components
export DATABASE_URL="${DATABASE_URL:-postgres://${BPC_DB_USER:-bpc}:${BPC_DB_PASSWORD:-bpc}@${BPC_DB_HOST:-localhost}:${BPC_DB_PORT:-5432}/${BPC_DB_NAME:-bpc}?sslmode=${BPC_DB_SSLMODE:-disable}}"

echo "Running migrations..."
echo "Database: ${BPC_DB_HOST:-localhost}:${BPC_DB_PORT:-5432}/${BPC_DB_NAME:-bpc}"

dbmate up

echo "Migrations complete."
