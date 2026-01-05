{-# LANGUAGE OverloadedStrings #-}

-- | Application Monad and Environment
--
-- Defines the AppM monad transformer stack and environment
-- for the BPC API server.
--
-- @since 0.1.0.0
module BPC.API.App
  ( -- * Environment
    Env(..)
  , mkEnv
    -- * App Monad
  , AppM
  , runAppM
    -- * Utilities
  , getEnv
  , withPool
  , logInfo
  , logError
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple (Connection)

import BPC.API.Error (AppError(..))
import BPC.DB (DBConfig, mkPool, withConn, closePool)

-- | Application environment.
data Env = Env
  { envPool :: Pool Connection
  -- ^ Database connection pool
  , envPort :: Int
  -- ^ Server port (default: 8080)
  , envLogLevel :: Text
  -- ^ Logging level (DEBUG, INFO, WARN, ERROR)
  , envApiKeyPepper :: Text
  -- ^ Pepper for API key hashing
  , envCorsOrigins :: [Text]
  -- ^ Allowed CORS origins
  , envRateLimitRps :: Int
  -- ^ Rate limit requests per second
  }

-- | Create application environment from pool and configuration.
--
-- Takes a pre-created database pool, port, and API key pepper.
-- The pepper must be provided externally (from environment variables).
--
-- @since 0.1.0.0
mkEnv :: Pool Connection -> Int -> Text -> IO (Either AppError Env)
mkEnv pool port pepper = pure $ Right Env
  { envPool = pool
  , envPort = port
  , envLogLevel = "INFO"
  , envApiKeyPepper = pepper
  , envCorsOrigins = ["*"]
  , envRateLimitRps = 100
  }

-- | Application monad transformer stack.
--
-- ReaderT Env for environment access
-- ExceptT AppError for error handling
-- IO for effects
type AppM = ReaderT Env (ExceptT AppError IO)

-- | Run an AppM action with the given environment.
--
-- @since 0.1.0.0
runAppM :: Env -> AppM a -> IO (Either AppError a)
runAppM env action = runExceptT $ runReaderT action env

-- | Get the environment.
getEnv :: AppM Env
getEnv = asks id

-- | Execute an action with a database connection from the pool.
withPool :: (Connection -> IO a) -> AppM a
withPool action = do
  pool <- asks envPool
  liftIO $ withConn pool action

-- | Log an info message.
logInfo :: Text -> AppM ()
logInfo msg = do
  level <- asks envLogLevel
  when (level `elem` ["DEBUG", "INFO"]) $
    liftIO $ TIO.putStrLn $ "[INFO] " <> msg
  where
    when True m = m
    when False _ = pure ()

-- | Log an error message.
logError :: Text -> AppM ()
logError msg = liftIO $ TIO.putStrLn $ "[ERROR] " <> msg
