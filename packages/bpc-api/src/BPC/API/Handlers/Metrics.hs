{-# LANGUAGE OverloadedStrings #-}

-- | Metrics Handlers (Prometheus Format)
module BPC.API.Handlers.Metrics
  ( metricsHandler
  , Metrics(..)
  , initMetrics
  , recordRequest
  , recordLatency
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

import BPC.API.App (AppM)

-- | Metrics state.
data Metrics = Metrics
  { mRequestCount :: IORef (Map Text Int)
  -- ^ Request counts by endpoint
  , mLatencies :: IORef [(Text, NominalDiffTime)]
  -- ^ Latency samples by endpoint
  , mErrorCount :: IORef Int
  -- ^ Total error count
  , mActiveConnections :: IORef Int
  -- ^ Current active connections
  }

-- | Initialize metrics state.
initMetrics :: IO Metrics
initMetrics = Metrics
  <$> newIORef Map.empty
  <*> newIORef []
  <*> newIORef 0
  <*> newIORef 0

-- | Record a request.
recordRequest :: Metrics -> Text -> IO ()
recordRequest m endpoint =
  modifyIORef' (mRequestCount m) $
    Map.insertWith (+) endpoint 1

-- | Record request latency.
recordLatency :: Metrics -> Text -> NominalDiffTime -> IO ()
recordLatency m endpoint latency =
  modifyIORef' (mLatencies m) $
    \xs -> (endpoint, latency) : take 9999 xs

-- | Metrics handler (Prometheus format).
--
-- GET /metrics
-- Returns metrics in Prometheus exposition format.
--
-- @since 0.1.0.0
metricsHandler :: Metrics -> AppM Text
metricsHandler metrics = liftIO $ do
  requestCounts <- readIORef (mRequestCount metrics)
  errorCount <- readIORef (mErrorCount metrics)
  activeConns <- readIORef (mActiveConnections metrics)
  latencies <- readIORef (mLatencies metrics)

  let requestLines = map formatRequestCount $ Map.toList requestCounts
  let latencyLines = formatLatencies latencies
  let errorLine = "bpc_errors_total " <> T.pack (show errorCount)
  let connLine = "bpc_active_connections " <> T.pack (show activeConns)

  pure $ T.unlines $
    [ "# HELP bpc_requests_total Total HTTP requests"
    , "# TYPE bpc_requests_total counter"
    ] ++ requestLines ++
    [ ""
    , "# HELP bpc_errors_total Total errors"
    , "# TYPE bpc_errors_total counter"
    , errorLine
    , ""
    , "# HELP bpc_active_connections Current active connections"
    , "# TYPE bpc_active_connections gauge"
    , connLine
    , ""
    , "# HELP bpc_request_duration_seconds Request duration histogram"
    , "# TYPE bpc_request_duration_seconds histogram"
    ] ++ latencyLines
  where
    formatRequestCount (endpoint, count) =
      "bpc_requests_total{endpoint=\"" <> endpoint <> "\"} " <> T.pack (show count)

    formatLatencies :: [(Text, NominalDiffTime)] -> [Text]
    formatLatencies xs =
      let grouped = Map.fromListWith (++) $ map (\(e, l) -> (e, [l])) xs
          buckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
      in concatMap (formatEndpointLatency buckets) $ Map.toList grouped

    formatEndpointLatency buckets (endpoint, latencies) =
      let counts = map (\b -> (b, length $ filter (<= realToFrac b) latencies)) buckets
          total = length latencies
          sum' = sum $ map realToFrac latencies :: Double
      in [ "bpc_request_duration_seconds_bucket{endpoint=\"" <> endpoint <>
           "\",le=\"" <> T.pack (show b) <> "\"} " <> T.pack (show c)
         | (b, c) <- counts
         ] ++
         [ "bpc_request_duration_seconds_bucket{endpoint=\"" <> endpoint <> "\",le=\"+Inf\"} " <> T.pack (show total)
         , "bpc_request_duration_seconds_sum{endpoint=\"" <> endpoint <> "\"} " <> T.pack (show sum')
         , "bpc_request_duration_seconds_count{endpoint=\"" <> endpoint <> "\"} " <> T.pack (show total)
         ]
