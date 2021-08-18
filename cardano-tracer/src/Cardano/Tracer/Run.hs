{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  -- | For testing purposes.
  , runCardanoTracerWithConfig
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Acceptors (runAcceptors)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers (runHandlers)
import           Cardano.Tracer.Types

runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} =
  readTracerConfig tracerConfig >>= runCardanoTracerWithConfig

runCardanoTracerWithConfig :: TracerConfig -> IO ()
runCardanoTracerWithConfig config = do
  acceptedMetrics  <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo
  concurrently_ (runAcceptors config acceptedMetrics acceptedNodeInfo)
                (runHandlers  config acceptedMetrics acceptedNodeInfo)
