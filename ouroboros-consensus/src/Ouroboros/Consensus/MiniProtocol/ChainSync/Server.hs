{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server (
    Tip
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncHeadersServer
    -- * Trace events
  , TraceChainSyncServerEvent (..)
  , TraceChainSyncServerInfo (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Block (ChainUpdate (..), Serialised,
                     Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, Follower,
                     WithPoint (..), getSerialisedBlockWithPoint,
                     getSerialisedHeaderWithPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)


chainSyncHeaderServerFollower
    :: ChainDB m blk
    -> ChainDB.ChainType
    -> ResourceRegistry m
    -> m (Follower m blk (WithPoint blk (SerialisedHeader blk)))
chainSyncHeaderServerFollower chainDB chainType registry =
  ChainDB.newFollower chainDB registry chainType getSerialisedHeaderWithPoint

chainSyncBlockServerFollower
    :: ChainDB m blk
    -> ResourceRegistry m
    -> m (Follower m blk (WithPoint blk (Serialised blk)))
chainSyncBlockServerFollower chainDB registry =
  ChainDB.newFollower chainDB registry ChainDB.SelectedChain getSerialisedBlockWithPoint

-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
--
chainSyncHeadersServer
    :: forall m blk.
       ( IOLike m
       , HasHeader (Header blk)
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Follower m blk (WithPoint blk (SerialisedHeader blk))
    -> ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB flr =
    chainSyncServerForFollower tracer chainDB flr

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
chainSyncBlocksServer
    :: forall m blk. (IOLike m, HasHeader (Header blk))
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Follower m blk (WithPoint blk (Serialised blk))
    -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB flr =
    chainSyncServerForFollower tracer chainDB flr

-- | A chain sync server.
--
-- This is a version of
-- 'Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample' that
-- uses a 'chainDB' and a 'Follower' instead of
-- 'Ourboros.Network.ChainProducerState.ChainProducerState'.
--
-- All the hard work is done by the 'Follower's provided by the 'ChainDB'.
--
chainSyncServerForFollower ::
     forall m blk b.
     ( IOLike m
     , HasHeader (Header blk)
     )
  => Tracer m (TraceChainSyncServerEvent blk)
  -> ChainDB m blk
  -> Follower  m blk (WithPoint blk b)
  -> ChainSyncServer b (Point blk) (Tip blk) m ()
chainSyncServerForFollower tracer chainDB flr =
    idle'
  where
    idle :: ServerStIdle b (Point blk) (Tip blk) m ()
    idle = ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = pure ()
      }

    idle' :: ChainSyncServer b (Point blk) (Tip blk) m ()
    idle' = ChainSyncServer $ return idle

    handleRequestNext :: m (Either (ServerStNext b (Point blk) (Tip blk) m ())
                                (m (ServerStNext b (Point blk) (Tip blk) m ())))
    handleRequestNext = ChainDB.followerInstruction flr >>= \case
      Just update -> do
        tip <- atomically $ ChainDB.getCurrentTip chainDB
        let traceInfo = TraceChainSyncServerInfo tip (point <$> update)
        traceWith tracer $ TraceChainSyncServerRead traceInfo
        return $ Left $ sendNext traceInfo update
      Nothing     -> return $ Right $ do
        -- Follower is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.followerInstructionBlocking flr
        tip    <- atomically $ ChainDB.getCurrentTip chainDB
        let traceInfo = TraceChainSyncServerInfo tip (point <$> update)
        traceWith tracer $ TraceChainSyncServerReadBlocked traceInfo
        return $ sendNext traceInfo update

    sendNext :: TraceChainSyncServerInfo blk
             -> ChainUpdate blk (WithPoint blk b)
             -> ServerStNext b (Point blk) (Tip blk) m ()
    sendNext traceInfo@(TraceChainSyncServerInfo tip _) = \case
        AddBlock hdr -> SendMsgRollForward (withoutPoint hdr) tip traceThenIdle
        RollBack pt  -> SendMsgRollBackward pt tip traceThenIdle
      where
        traceThenIdle = ChainSyncServer $ do
          traceWith tracer $ TraceChainSyncServerSent traceInfo
          return idle

    handleFindIntersect :: [Point blk]
                        -> m (ServerStIntersect b (Point blk) (Tip blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.followerForward flr points
      tip     <- atomically $ ChainDB.getCurrentTip chainDB
      case changed of
        Just pt -> return $ SendMsgIntersectFound pt tip idle'
        Nothing -> return $ SendMsgIntersectNotFound tip idle'

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Server.
data TraceChainSyncServerEvent blk =
    -- | Read a chain update instruction non-blockingly.
    TraceChainSyncServerRead        (TraceChainSyncServerInfo blk)
    -- | Read a chain update instruction blockingly.
  | TraceChainSyncServerReadBlocked (TraceChainSyncServerInfo blk)
    -- | Sent a chain update instruction.
  | TraceChainSyncServerSent        (TraceChainSyncServerInfo blk)
  deriving (Eq, Show)

data TraceChainSyncServerInfo blk =
    TraceChainSyncServerInfo
      (Tip blk)
      -- ^ Tip of the currently selected chain.
      (ChainUpdate blk (Point blk))
      -- ^ The whole headers/blocks in the traced 'ChainUpdate' are substituted
      -- with their corresponding 'Point'.
  deriving (Eq, Show)
