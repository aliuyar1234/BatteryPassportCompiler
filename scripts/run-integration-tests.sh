#!/usr/bin/env bash
# BPC Integration Tests Runner
# Starts test database, runs migrations, and executes integration tests

set -euo pipefail

# Configuration
export BPC_DB_HOST=localhost
export BPC_DB_PORT=55432
export BPC_DB_USER=bpc
export BPC_DB_PASSWORD=bpc
export BPC_DB_NAME=bpc_test

cleanup() {
  echo "Stopping test database..."
  docker compose -f docker-compose.test.yml down -v
}

trap cleanup EXIT

echo "Starting test database..."
docker compose -f docker-compose.test.yml up -d

echo "Waiting for test database to be healthy..."
sleep 3

until docker compose -f docker-compose.test.yml exec -T postgres pg_isready -U bpc -d bpc_test > /dev/null 2>&1; do
  echo "Waiting for PostgreSQL..."
  sleep 1
done

echo "Running migrations on test database..."
./scripts/migrate.sh

echo ""
echo "Running integration tests..."
echo ""

cabal test bpc-db:integration
cabal test bpc-api:integration
cabal test bpc-worker:integration

echo ""
echo "Integration tests complete!"
