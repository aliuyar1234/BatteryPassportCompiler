{-# LANGUAGE OverloadedStrings #-}

-- | BPC API Server
--
-- RESTful API for the Battery Passport Compiler.
--
-- @since 0.1.0.0
module BPC.API
  ( -- * Application
    app
  , runServer
    -- * Configuration
  , Env(..)
  , mkEnv
    -- * Re-exports
  , module BPC.API.App
  , module BPC.API.Error
  , module BPC.API.Types
  , module BPC.API.Routes
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import BPC.API.App
import BPC.API.Error
import BPC.API.Types
import BPC.API.Routes (app, routes)

-- | Run the API server.
--
-- @since 0.1.0.0
runServer :: Env -> IO ()
runServer env = do
  putStrLn $ "Starting BPC API server on port " ++ show (envPort env)
  run (envPort env) (app env)

-- | Create environment from configuration.
--
-- @since 0.1.0.0
mkEnv :: Pool Connection -> Int -> Text -> IO Env
mkEnv pool port pepper = pure Env
  { envPool = pool
  , envPort = port
  , envApiKeyPepper = pepper
  , envCorrelationId = Nothing
  }
