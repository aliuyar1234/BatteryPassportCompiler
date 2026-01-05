# Docker Deployment Guide

This directory contains Docker configurations for the Battery Passport Compiler (BPC) project.

## Prerequisites

- Docker 20.10 or later
- Docker Compose 2.0 or later
- At least 4GB of available RAM
- At least 10GB of available disk space

## Quick Start

### Development Environment

1. Start all services (PostgreSQL, RabbitMQ, API, Worker):

```bash
docker compose up -d
```

2. Check service health:

```bash
docker compose ps
```

3. View logs:

```bash
# All services
docker compose logs -f

# Specific service
docker compose logs -f bpc-api
docker compose logs -f bpc-worker
```

4. Stop all services:

```bash
docker compose down
```

### Database Setup

After starting the services, you need to run migrations:

```bash
# Run migrations (you'll need dbmate installed or use docker exec)
./scripts/migrate.sh
```

Alternatively, seed development data:

```bash
./scripts/seed-dev.sh
```

## Services

### bpc-api

The HTTP API server.

- **Port**: 8080
- **Health Check**: http://localhost:8080/health
- **Dependencies**: PostgreSQL

**Environment Variables**:
- `DATABASE_URL`: PostgreSQL connection string
- `PORT`: HTTP server port (default: 8080)
- `LOG_LEVEL`: Logging level (default: info)

### bpc-worker

The background job processing worker.

- **Dependencies**: PostgreSQL, RabbitMQ

**Environment Variables**:
- `DATABASE_URL`: PostgreSQL connection string
- `RABBITMQ_URL`: RabbitMQ connection string
- `LOG_LEVEL`: Logging level (default: info)
- `WORKER_CONCURRENCY`: Number of concurrent workers (default: 4)

### PostgreSQL

Database service.

- **Port**: 5432
- **Database**: bpc
- **User**: bpc
- **Password**: bpc (change in production!)

### RabbitMQ

Message queue service.

- **AMQP Port**: 5672
- **Management UI**: http://localhost:15672
- **Default Credentials**: guest/guest (change in production!)

## Building Images

### Build all images:

```bash
docker compose build
```

### Build specific service:

```bash
docker compose build bpc-api
docker compose build bpc-worker
```

### Build with no cache:

```bash
docker compose build --no-cache
```

## Production Deployment

For production deployments, consider:

1. **Use environment files**: Create a `.env` file for sensitive configuration
2. **Change default passwords**: Update PostgreSQL and RabbitMQ credentials
3. **Enable TLS**: Configure HTTPS for the API
4. **Resource limits**: Set CPU and memory limits in docker-compose.yml
5. **Persistent volumes**: Ensure data volumes are backed up
6. **Monitoring**: Add monitoring and alerting
7. **Log aggregation**: Configure centralized logging

### Example Production Override

Create a `docker-compose.prod.yml`:

```yaml
services:
  bpc-api:
    environment:
      LOG_LEVEL: warn
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G

  bpc-worker:
    environment:
      LOG_LEVEL: warn
      WORKER_CONCURRENCY: 8
    deploy:
      resources:
        limits:
          cpus: '4'
          memory: 4G
        reservations:
          cpus: '2'
          memory: 2G
```

Run with:

```bash
docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d
```

## Troubleshooting

### Service won't start

Check logs:
```bash
docker compose logs <service-name>
```

### Database connection errors

Ensure PostgreSQL is healthy:
```bash
docker compose ps postgres
docker compose exec postgres pg_isready -U bpc
```

### Worker not processing jobs

1. Check RabbitMQ is running:
```bash
docker compose ps rabbitmq
```

2. Check RabbitMQ management UI: http://localhost:15672

3. Verify worker logs:
```bash
docker compose logs -f bpc-worker
```

### Out of disk space

Clean up Docker resources:
```bash
docker system prune -a --volumes
```

## Maintenance

### Update images

```bash
docker compose pull
docker compose up -d
```

### Backup database

```bash
docker compose exec postgres pg_dump -U bpc bpc > backup.sql
```

### Restore database

```bash
cat backup.sql | docker compose exec -T postgres psql -U bpc bpc
```

### Clean up

Remove all containers and volumes:
```bash
docker compose down -v
```

## CI/CD Integration

The project includes GitHub Actions workflows in `.github/workflows/`:

- **ci.yml**: Runs build, test, lint, and security audit on every push/PR

The CI pipeline:
1. Builds the project with GHC 9.6.4
2. Runs all tests (unit + integration)
3. Checks code formatting with fourmolu
4. Runs HLint for code quality
5. Performs security audit with cabal-audit

## File Structure

```
.
├── .github/
│   └── workflows/
│       └── ci.yml           # GitHub Actions CI pipeline
├── Dockerfile.api           # Multi-stage build for bpc-api
├── Dockerfile.worker        # Multi-stage build for bpc-worker
├── docker-compose.yml       # Development docker compose configuration
└── .dockerignore           # Files to exclude from Docker builds
```

## Support

For issues or questions, please check the project documentation or open an issue on GitHub.
