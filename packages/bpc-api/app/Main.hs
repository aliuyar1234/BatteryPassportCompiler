{-# LANGUAGE OverloadedStrings #-}

-- | BPC API Server Entry Point
--
-- Main executable for the Battery Passport Compiler API.
--
-- @since 0.1.0.0
module Main (main) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import qualified BPC.DB as DB
import BPC.API (runServer, mkEnv)

-- | Minimum required length for API key pepper (32 bytes).
minPepperLength :: Int
minPepperLength = 32

-- | Main entry point.
main :: IO ()
main = do
  putStrLn "BPC API Server starting..."

  -- Load and validate configuration from environment
  configResult <- loadConfig
  case configResult of
    Left err -> do
      putStrLn $ "Configuration error: " ++ err
      exitFailure
    Right (dbConfig, serverConfig) -> do
      -- Create database pool
      poolResult <- DB.mkPool dbConfig
      case poolResult of
        Left err -> do
          putStrLn $ "Failed to create database pool: " ++ show err
          exitFailure
        Right pool -> do
          -- Run server with cleanup
          bracket
            (pure pool)
            DB.closePool
            (\p -> do
              envResult <- mkEnv p (scPort serverConfig) (scApiKeyPepper serverConfig)
              case envResult of
                Left err -> do
                  putStrLn $ "Failed to create environment: " ++ show err
                  exitFailure
                Right env -> runServer env
            )

-- | Server configuration (separate from DB config).
data ServerConfig = ServerConfig
  { scPort :: !Int
  , scApiKeyPepper :: !T.Text
  , scWebhookEncryptionKey :: !(Maybe T.Text)
  , scSigningMode :: !SigningMode
  }
  deriving stock (Show, Eq)

-- | Signing key provider mode.
data SigningMode
  = SigningModeEnv       -- ^ Load key from environment variable (development)
  | SigningModeHSM       -- ^ Use HSM for signing (production)
  deriving stock (Show, Eq)

-- | Load configuration from environment with validation.
loadConfig :: IO (Either String (DB.DBConfig, ServerConfig))
loadConfig = do
  -- Database config
  host <- fromMaybe "localhost" <$> lookupEnv "BPC_DB_HOST"
  port <- fromMaybe 5432 . (>>= readMaybe) <$> lookupEnv "BPC_DB_PORT"
  name <- fromMaybe "bpc" <$> lookupEnv "BPC_DB_NAME"
  user <- fromMaybe "bpc" <$> lookupEnv "BPC_DB_USER"
  password <- fromMaybe "" <$> lookupEnv "BPC_DB_PASSWORD"
  poolSize <- fromMaybe 10 . (>>= readMaybe) <$> lookupEnv "BPC_DB_POOL_SIZE"

  -- Server config with validation
  apiPort <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "BPC_API_PORT"
  pepperEnv <- lookupEnv "BPC_API_KEY_PEPPER"
  webhookKeyEnv <- lookupEnv "BPC_WEBHOOK_ENCRYPTION_KEY"
  signingModeEnv <- fromMaybe "env" <$> lookupEnv "BPC_SIGNING_MODE"

  -- Validate pepper is set and meets minimum length
  case pepperEnv of
    Nothing -> pure $ Left "BPC_API_KEY_PEPPER environment variable is required"
    Just pepper
      | length pepper < minPepperLength ->
          pure $ Left $ "BPC_API_KEY_PEPPER must be at least " ++ show minPepperLength ++ " bytes"
      | otherwise -> do
          let signingMode = case signingModeEnv of
                "hsm" -> SigningModeHSM
                _     -> SigningModeEnv

          let dbConfig = DB.DBConfig
                { DB.dbHost = T.pack host
                , DB.dbPort = port
                , DB.dbName = T.pack name
                , DB.dbUser = T.pack user
                , DB.dbPassword = T.pack password
                , DB.dbPoolSize = poolSize
                , DB.dbPoolTimeout = 30
                }

          let serverConfig = ServerConfig
                { scPort = apiPort
                , scApiKeyPepper = T.pack pepper
                , scWebhookEncryptionKey = T.pack <$> webhookKeyEnv
                , scSigningMode = signingMode
                }

          pure $ Right (dbConfig, serverConfig)
