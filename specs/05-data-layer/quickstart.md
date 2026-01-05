# Quickstart: Data Layer

**Feature**: 05-data-layer
**Date**: 2025-12-28
**Package**: bpc-db

## Prerequisites

- 01-foundation complete (database running)
- Migrations applied (`./scripts/migrate.sh`)

## Quick Usage

### Create Connection Pool

```haskell
import BPC.DB.Pool

main :: IO ()
main = do
  let config = DBConfig
        { dbHost = "localhost"
        , dbPort = 5432
        , dbUser = "bpc"
        , dbPassword = "bpc"
        , dbName = "bpc"
        , dbPoolSize = 10
        }

  pool <- mkPool config

  -- Use pool
  withConn pool $ \conn -> do
    docs <- listDocuments conn tenantId defaultListOptions
    print docs
```

### Repository Usage

```haskell
import BPC.DB.Repos.Documents
import BPC.DB.Repos.Snapshots
import BPC.DB.Repos.Passports

-- Create document
docId <- withConn pool $ \conn ->
  createDocument conn tenantId CreateDocumentInput
    { cdiKind = BOM
    , cdiExternalRef = Just "bom-001"
    }

-- Upload version
dvId <- withConn pool $ \conn ->
  uploadVersion conn tenantId docId UploadInput
    { uiContent = bomBytes
    , uiMimeType = "application/json"
    }

-- Create and seal snapshot
snapId <- withConn pool $ \conn -> do
  sid <- createSnapshot conn tenantId Nothing
  addItem conn tenantId sid factId1
  addItem conn tenantId sid factId2
  sealSnapshot conn tenantId sid
  pure sid
```

### Event Store

```haskell
import BPC.DB.Repos.Events

-- Append event
eventId <- withConn pool $ \conn ->
  appendEvent conn AppendEventInput
    { aeiTenantId = tenantId
    , aeiAggregateType = "Passport"
    , aeiAggregateId = unPassportId passportId
    , aeiAggregateVersion = 1
    , aeiEventType = "PassportCreated"
    , aeiActorId = Just actorId
    , aeiPayload = object ["sku" .= ("SKU-123" :: Text)]
    }

-- Verify chain integrity
withConn pool $ \conn -> do
  result <- verifyChain conn tenantId
  case result of
    Right () -> putStrLn "Chain verified!"
    Left err -> error $ "Chain corrupted: " ++ show err
```

### Job Queue

```haskell
import BPC.DB.Repos.Jobs

-- Enqueue job
jobId <- withConn pool $ \conn ->
  enqueue conn tenantId EnqueueInput
    { eiType = COMPILE_PASSPORT
    , eiPayload = object ["passport_id" .= passportId]
    , eiIdempotencyKey = "compile-" <> show passportId
    , eiPriority = 100
    }

-- Acquire and process job (in worker)
worker :: Pool Connection -> Text -> IO ()
worker pool workerId = forever $ do
  mJob <- withConn pool $ \conn -> acquireLease conn workerId
  case mJob of
    Nothing -> threadDelay 1000000  -- Poll interval
    Just job -> do
      result <- processJob job
      withConn pool $ \conn ->
        case result of
          Right () -> complete conn (jTenantId job) (jJobId job)
          Left err -> fail conn (jTenantId job) (jJobId job) (toJSON err)
```

## Testing

### Run Integration Tests

```bash
# Start test database
docker compose -f docker-compose.test.yml up -d

# Wait for postgres
sleep 5

# Run migrations
DATABASE_URL="postgres://bpc:bpc@localhost:55432/bpc_test?sslmode=disable" dbmate up

# Run tests
BPC_DB_HOST=localhost BPC_DB_PORT=55432 BPC_DB_NAME=bpc_test cabal test bpc-db:integration
```

### Test Event Chain

```haskell
testEventChain :: Spec
testEventChain = describe "Event Store" $ do
  it "maintains hash chain" $ do
    -- Append events
    e1 <- appendEvent conn input1
    e2 <- appendEvent conn input2

    -- Verify chain
    verifyChain conn tenantId `shouldReturn` Right ()

  it "detects tampering" $ do
    -- Manually corrupt a hash
    execute conn "UPDATE events SET event_hash = ? WHERE event_id = ?" (wrongHash, e1)

    -- Verify should fail
    verifyChain conn tenantId `shouldReturn` Left ChainCorrupted
```

## Common Patterns

### Handle Not Found

```haskell
getPassportOrFail :: Connection -> TenantId -> PassportId -> IO Passport
getPassportOrFail conn tenantId passportId = do
  mPassport <- getPassport conn tenantId passportId
  case mPassport of
    Just p -> pure p
    Nothing -> throwIO $ AppError NOT_FOUND "Passport not found"
```

### Transaction Wrapper

```haskell
import Database.PostgreSQL.Simple.Transaction (withTransaction)

atomically :: Pool Connection -> (Connection -> IO a) -> IO a
atomically pool action = withConn pool $ \conn ->
  withTransaction conn (action conn)

-- Usage
atomically pool $ \conn -> do
  -- All operations in single transaction
  docId <- createDocument conn tenantId input
  dvId <- uploadVersion conn tenantId docId versionInput
  pure (docId, dvId)
```

## Next Steps

After data layer is complete:

1. **06-api-server**: Uses repositories for handlers
2. **07-job-processing**: Uses job queue for async processing
