{-# LANGUAGE OverloadedStrings #-}

-- | BPC Worker Entry Point
--
-- Main executable for the Battery Passport Compiler worker.
--
-- @since 0.1.0.0
module Main (main) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import qualified BPC.DB as DB
import BPC.Worker (loadConfig, startWorker)

-- | Main entry point.
main :: IO ()
main = do
  putStrLn "BPC Worker starting..."

  -- Load database configuration
  dbConfig <- loadDbConfig

  -- Load worker configuration
  workerConfig <- loadConfig

  -- Create database pool
  poolResult <- DB.mkPool dbConfig
  case poolResult of
    Left err -> do
      putStrLn $ "Failed to create database pool: " ++ show err
      error "Database initialization failed"
    Right pool -> do
      -- Run worker with cleanup
      bracket
        (pure pool)
        DB.closePool
        (\p -> startWorker workerConfig p)

-- | Load database configuration from environment.
loadDbConfig :: IO DB.DBConfig
loadDbConfig = do
  host <- fromMaybe "localhost" <$> lookupEnv "BPC_DB_HOST"
  port <- fromMaybe 5432 . (>>= readMaybe) <$> lookupEnv "BPC_DB_PORT"
  name <- fromMaybe "bpc" <$> lookupEnv "BPC_DB_NAME"
  user <- fromMaybe "bpc" <$> lookupEnv "BPC_DB_USER"
  password <- fromMaybe "" <$> lookupEnv "BPC_DB_PASSWORD"
  poolSize <- fromMaybe 5 . (>>= readMaybe) <$> lookupEnv "BPC_DB_POOL_SIZE"

  pure DB.DBConfig
    { DB.dbHost = T.pack host
    , DB.dbPort = port
    , DB.dbName = T.pack name
    , DB.dbUser = T.pack user
    , DB.dbPassword = T.pack password
    , DB.dbPoolSize = poolSize
    , DB.dbPoolTimeout = 30
    , DB.dbConnectTimeout = 10
    }
