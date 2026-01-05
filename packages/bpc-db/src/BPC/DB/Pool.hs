{-# LANGUAGE OverloadedStrings #-}

-- | Database connection pool management.
--
-- Provides configurable connection pooling using resource-pool,
-- with proper error handling and connection lifecycle management.
module BPC.DB.Pool
  ( -- * Configuration
    DBConfig(..)
  , defaultDBConfig
    -- * Pool Operations
  , mkPool
  , withConn
  , closePool
    -- * Connection String
  , buildConnString
  ) where

import Control.Exception (SomeException, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Error (DBError(..))

-- | Database connection configuration.
data DBConfig = DBConfig
  { dbHost :: Text
  -- ^ Database host (default: localhost)
  , dbPort :: Int
  -- ^ Database port (default: 5432)
  , dbName :: Text
  -- ^ Database name
  , dbUser :: Text
  -- ^ Database user
  , dbPassword :: Text
  -- ^ Database password
  , dbPoolSize :: Int
  -- ^ Maximum number of connections (default: 10)
  , dbPoolTimeout :: NominalDiffTime
  -- ^ Connection idle timeout in seconds (default: 30)
  , dbConnectTimeout :: Int
  -- ^ Connection timeout in seconds (default: 10)
  }
  deriving stock (Show, Eq, Generic)

-- | Default database configuration.
--
-- Uses localhost:5432, pool size 10, 30s idle timeout.
defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig
  { dbHost = "localhost"
  , dbPort = 5432
  , dbName = "bpc"
  , dbUser = "bpc"
  , dbPassword = ""
  , dbPoolSize = 10
  , dbPoolTimeout = 30
  , dbConnectTimeout = 10
  }

-- | Build PostgreSQL connection string from config.
--
-- Format: host=HOST port=PORT dbname=NAME user=USER password=PASS
buildConnString :: DBConfig -> ByteString
buildConnString cfg = BS.intercalate " "
  [ "host=" <> encodeText (dbHost cfg)
  , "port=" <> BS.pack (show (dbPort cfg))
  , "dbname=" <> encodeText (dbName cfg)
  , "user=" <> encodeText (dbUser cfg)
  , "password=" <> encodeText (dbPassword cfg)
  , "connect_timeout=" <> BS.pack (show (dbConnectTimeout cfg))
  ]
  where
    encodeText :: Text -> ByteString
    encodeText = TE.encodeUtf8

-- | Create a new connection pool.
--
-- Creates a pool of PostgreSQL connections based on the provided config.
-- Returns DB_UNAVAILABLE error if initial connection fails.
--
-- @since 0.1.0.0
mkPool :: DBConfig -> IO (Either DBError (Pool Connection))
mkPool cfg = do
  result <- tryConnect
  case result of
    Left err -> pure $ Left $ DB_UNAVAILABLE $ T.pack $ show err
    Right conn -> do
      -- Initial connection successful, close it and create pool
      PG.close conn
      pool <- Pool.newPool $ Pool.defaultPoolConfig
        (PG.connectPostgreSQL connStr)  -- create connection
        PG.close                         -- destroy connection
        (realToFrac $ dbPoolTimeout cfg) -- idle timeout
        (dbPoolSize cfg)                 -- max connections
      pure $ Right pool
  where
    connStr = buildConnString cfg

    tryConnect :: IO (Either SomeException Connection)
    tryConnect =
      (Right <$> PG.connectPostgreSQL connStr) `catch`
        (\(e :: SomeException) -> pure $ Left e)

-- | Execute an action with a connection from the pool.
--
-- Automatically returns the connection to the pool after use,
-- even if an exception occurs.
--
-- @since 0.1.0.0
withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn pool action = Pool.withResource pool action

-- | Close all connections in the pool.
--
-- Should be called when shutting down the application.
--
-- @since 0.1.0.0
closePool :: Pool Connection -> IO ()
closePool = Pool.destroyAllResources
