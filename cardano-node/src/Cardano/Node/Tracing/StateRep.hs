{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.StateRep
  ( NodeState (..)
  , traceNodeStateChainDB
  , traceNodeStateStartup
  , traceNodeStateShutdown
  ) where

import           Cardano.Prelude
import           Cardano.Logging
import           Data.Aeson
-- import qualified Data.Map as Map
-- import           Data.Scientific (coefficient)
import           Network.Socket

--import           Ouroboros.Consensus.Ledger.Inspect
--import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Network.Magic
--import           Ouroboros.Network.NodeToClient.Version
import           Ouroboros.Network.NodeToNode.Version
import           Ouroboros.Network.PeerSelection.LedgerPeers
--import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Subscription

import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Startup
--import           Cardano.Node.TraceConstraints
import           Cardano.Tracing.OrphanInstances.Network ()

-- | The representation of the current state of node.
--   All node states prior to tracing system going online are effectively invisible.
data NodeState blk
  = NodeTracingOnlineConfiguring
  | NodeStartup (StartupTrace blk)
  | NodeShutdown ShutdownTrace

{-
data NodeState blk
  =
  | NodeChainDBOpening (ChainDB.TraceEvent blk)
  -- TraceOpenEvent              (TraceOpenEvent               blk)
      -- OpeningImmutableDB Pct
      -- OpenedImmutableDB
      -- OpeningVolatileDB Pct
      -- OpenedVolatileDB
      -- OpeningLgrDB Pct
  | NodeLedgerReplay
  -- TraceLedgerReplayEvent      (LedgerReplayEvent            blk)
      -- ReplayFromGenesis Pct
      -- ReplayFromSnapshot Pct
      -- ReplayedBlock
  -- TraceOpenEvent              (TraceOpenEvent               blk)
      -- OpenedLgrDB
  -- TraceInitChainSelEvent      (TraceInitChainSelEvent       blk)
      -- InitChainSelection Pct
      -- InitalChainSelected
  | NodeKernelOnlineSyncing         -- just before onKernel in rnNodeKernelHook
  -- | NodeSyncing (ChainDB.TraceEvent blk)
  -- TraceAddBlockEvent          (TraceAddBlockEvent           blk)
    -- ChainDB.AddedToCurrentChain
  -- | NodeStartup (StartupTrace blk)

-}

deriving instance Generic BasicInfoCommon
deriving instance Generic BasicInfoByron
deriving instance Generic BasicInfoNetwork
deriving instance Generic BasicInfoShelleyBased
deriving instance Generic DiffusionMode
deriving instance Generic DnsSubscriptionTarget
deriving instance Generic IPSubscriptionTarget
deriving instance Generic NodeToClientVersion
deriving instance Generic NodeToNodeVersion
deriving instance Generic SocketConfigError
deriving instance Generic (SocketOrSocketInfo SockAddr SockAddr)
deriving instance Generic (SocketOrSocketInfo LocalSocket LocalAddress)
deriving instance Generic UseLedgerAfter

deriving instance Generic (StartupTrace blk)

deriving instance Generic (NodeState blk)

instance ToJSONKey NodeToClientVersion
instance ToJSONKey NodeToNodeVersion

instance ToJSON PortNumber where
  toJSON = Number . fromInteger . toInteger

instance ToJSON Socket where
  toJSON = String . show

instance ToJSON IOException where
  toJSON = String . show

instance ToJSON BasicInfoByron
instance ToJSON BasicInfoCommon
instance ToJSON BasicInfoNetwork
instance ToJSON BasicInfoShelleyBased
instance ToJSON DiffusionMode
instance ToJSON DnsSubscriptionTarget
instance ToJSON IPSubscriptionTarget
instance ToJSON LocalSocket
instance ToJSON NetworkMagic
instance ToJSON SocketConfigError
instance ToJSON (SocketOrSocketInfo SockAddr SockAddr)
instance ToJSON (SocketOrSocketInfo LocalSocket LocalAddress)
instance ToJSON UseLedgerAfter

instance ( ToJSON (BlockNodeToClientVersion blk)
         , ToJSON (BlockNodeToNodeVersion blk)
         ) => ToJSON (StartupTrace blk)

instance ( ToJSON (BlockNodeToClientVersion blk)
         , ToJSON (BlockNodeToNodeVersion blk)
         ) => ToJSON (NodeState blk)

  {-
instance FromJSON Socket.PortNumber where
  parseJSON = withScientific "PortNumber" $ \s ->
    return ((fromInteger $ coefficient s) :: Socket.PortNumber)
  -}

-- | Strictly speaking, we mustn't provide 'FromJSON' instance here,
--   but it will be convenient for acceptor application.
--instance FromJSON StartupTraceEvent
--instance FromJSON (NodeState blk)

-- | ...
traceNodeStateChainDB
  :: --forall blk.
     --( TraceConstraints blk
     --, LedgerSupportsProtocol blk
     --, InspectLedger blk
     --)
     Trace IO (NodeState blk)
  -> ChainDB.TraceEvent blk
  -> IO ()
traceNodeStateChainDB _tr _ev =
  return ()
  {-
  case ev of
    ChainDB.TraceAddBlockEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceAddBlockEvent (show ev')
    ChainDB.TraceFollowerEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceFollowerEvent (show ev')
    ChainDB.TraceCopyToImmutableDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceCopyToImmutableDBEvent (show ev')
    ChainDB.TraceGCEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceGCEvent (show ev')
    ChainDB.TraceInitChainSelEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceInitChainSelEvent (show ev')
    ChainDB.TraceOpenEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceOpenEvent (show ev')
    ChainDB.TraceIteratorEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceIteratorEvent (show ev')
    ChainDB.TraceLedgerEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceLedgerEvent (show ev')
    ChainDB.TraceLedgerReplayEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceLedgerReplayEvent (show ev')
    ChainDB.TraceImmutableDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceImmutableDBEvent (show ev')
    ChainDB.TraceVolatileDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceVolatileDBEvent (show ev')
  -}

traceNodeStateStartup
  :: Trace IO (NodeState blk)
  -> StartupTrace blk
  -> IO ()
traceNodeStateStartup tr = traceWith tr . NodeStartup

traceNodeStateShutdown
  :: Trace IO (NodeState blk)
  -> ShutdownTrace
  -> IO ()
traceNodeStateShutdown tr = traceWith tr . NodeShutdown
