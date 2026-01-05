#!/usr/bin/env bash
# BPC Development Environment - Start
# Starts PostgreSQL and RabbitMQ containers

set -euo pipefail

echo "Starting development services..."
docker compose up -d

echo "Waiting for services to be healthy..."
sleep 3

# Wait for PostgreSQL
until docker compose exec -T postgres pg_isready -U bpc -d bpc > /dev/null 2>&1; do
  echo "Waiting for PostgreSQL..."
  sleep 1
done

echo ""
echo "Development services started:"
echo "  PostgreSQL: localhost:5432 (user: bpc, password: bpc, db: bpc)"
echo "  RabbitMQ:   localhost:5672 (Management UI: http://localhost:15672)"
echo ""
echo "Run './scripts/migrate.sh' to apply database migrations."
