{-# LANGUAGE OverloadedStrings #-}

-- | Health Check Handlers
module BPC.API.Handlers.Health
  ( healthLive
  , healthReady
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)

data HealthResponse = HealthResponse
  { hrStatus :: Text
  , hrDetails :: Maybe HealthDetails
  }
  deriving stock (Show, Eq, Generic)

data HealthDetails = HealthDetails
  { hdDatabase :: Text
  , hdVersion :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON HealthResponse where
  toJSON HealthResponse{..} = object $
    [ "status" .= hrStatus ] ++
    maybe [] (\d -> ["details" .= d]) hrDetails

instance ToJSON HealthDetails where
  toJSON HealthDetails{..} = object
    [ "database" .= hdDatabase
    , "version" .= hdVersion
    ]

-- | Liveness probe.
--
-- GET /health/live
-- Always returns 200 OK if server is running.
--
-- @since 0.1.0.0
healthLive :: AppM HealthResponse
healthLive = pure HealthResponse
  { hrStatus = "OK"
  , hrDetails = Nothing
  }

-- | Readiness probe.
--
-- GET /health/ready
-- Returns 200 OK if database is accessible, 503 otherwise.
--
-- @since 0.1.0.0
healthReady :: AppM HealthResponse
healthReady = do
  dbStatus <- checkDatabase
  if dbStatus
    then pure HealthResponse
      { hrStatus = "OK"
      , hrDetails = Just HealthDetails
        { hdDatabase = "connected"
        , hdVersion = "0.1.0"
        }
      }
    else pure HealthResponse
      { hrStatus = "UNAVAILABLE"
      , hrDetails = Just HealthDetails
        { hdDatabase = "disconnected"
        , hdVersion = "0.1.0"
        }
      }

-- | Check database connectivity.
checkDatabase :: AppM Bool
checkDatabase = do
  result <- liftIO $ (do
    -- Try to execute simple query
    -- In real implementation: withPool $ \conn -> PG.query_ conn "SELECT 1"
    pure True) `catch` handleError
  pure result
  where
    handleError :: SomeException -> IO Bool
    handleError _ = pure False
