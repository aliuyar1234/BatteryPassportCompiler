#!/usr/bin/env bash
# BPC Development Environment - Stop
# Stops PostgreSQL and RabbitMQ containers

set -euo pipefail

echo "Stopping development services..."
docker compose down

echo "Development services stopped."
