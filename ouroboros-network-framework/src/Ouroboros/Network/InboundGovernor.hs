{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Server implementation based on 'ConnectionManager'
--
module Ouroboros.Network.InboundGovernor
  ( InboundGovernorObservableState (..)
  , newObservableStateVar
  , newObservableStateVarIO
  , newObservableStateVarFromSeed
    -- * Run Inbound Protocol Governor
  , inboundGovernor
    -- * Trace
  , InboundGovernorTrace (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  , AcceptConnectionsPolicyTrace (..)
    -- * Re-exports
  , Transition' (..)
  , TransitionTrace' (..)
  ) where

import           Control.Applicative (Alternative)
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (SomeAsyncException (..), assert)
import           Control.Monad (foldM, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Cache
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid.Synchronisation
import           Data.Void (Void)

import qualified Network.Mux as Mux

import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.InformationChannel
                     (InboundGovernorInfoChannel)
import qualified Ouroboros.Network.ConnectionManager.InformationChannel as InfoChannel
import           Ouroboros.Network.ConnectionManager.Types hiding
                     (TrUnexpectedlyFalseAssertion)
import           Ouroboros.Network.Context
import           Ouroboros.Network.InboundGovernor.Event
import           Ouroboros.Network.InboundGovernor.State
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Server.RateLimiting

-- | Run the server, which consists of the following components:
--
-- * /inbound governor/, it corresponds to p2p-governor on outbound side
-- * /accept loop(s)/, one per given ip address.  We support up to one ipv4
--   address and up to one ipv6 address, i.e. an ipv6 enabled node will run two
--   accept loops on listening on different addresses with shared /inbound governor/.
--
-- The server can be run in either of two 'MuxMode'-es:
--
-- * 'InitiatorResponderMode'
-- * 'ResponderMode'
--
-- The first one is used in data diffusion for /Node-To-Node protocol/, while the
-- other is useful for running a server for the /Node-To-Client protocol/.
--
inboundGovernor :: forall (muxMode :: MuxMode) socket initiatorCtx peerAddr versionData versionNumber m a b.
                   ( Alternative (STM m)
                   , MonadAsync    m
                   , MonadCatch    m
                   , MonadEvaluate m
                   , MonadThrow    m
                   , MonadThrow    (STM m)
                   , MonadTime     m
                   , MonadTimer    m
                   , MonadMask     m
                   , Ord peerAddr
                   , HasResponder muxMode ~ True
                   )
                => Tracer m (RemoteTransitionTrace peerAddr)
                -> Tracer m (InboundGovernorTrace peerAddr)
                -> InboundGovernorInfoChannel muxMode initiatorCtx peerAddr versionData ByteString m a b
                -> Maybe DiffTime -- protocol idle timeout
                -> MuxConnectionManager muxMode socket initiatorCtx (ResponderContext peerAddr) peerAddr versionData versionNumber ByteString m a b
                -> StrictTVar m InboundGovernorObservableState
                -> m Void
inboundGovernor trTracer tracer inboundInfoChannel
                inboundIdleTimeout connectionManager
                observableStateVar = do
    -- State needs to be a TVar, otherwise, when catching the exception inside
    -- the loop we do not have access to the most recent version of the state
    -- and might be truncating transitions.
    st <- newTVarIO emptyState
    inboundGovernorLoop st
     `catch`
       (\(e :: SomeException) -> do
         state <- readTVarIO st
         _ <- Map.traverseWithKey
               (\connId _ -> do
                 -- Remove the connection from the state so
                 -- mkRemoteTransitionTrace can create the correct state
                 -- transition to Nothing value.
                 let state' = unregisterConnection connId state
                 traceWith trTracer
                           (mkRemoteTransitionTrace connId state state')
               )
               (igsConnections state)
         traceWith tracer (TrInboundGovernorError e)
         throwIO e
       )
  where
    emptyState :: InboundGovernorState muxMode initiatorCtx peerAddr m a b
    emptyState = InboundGovernorState {
            igsConnections   = Map.empty,
            igsObservableVar = observableStateVar,
            igsCountersCache = mempty
          }

    -- The inbound protocol governor recursive loop.  The 'igsConnections' is
    -- updated as we recurse.
    --
    inboundGovernorLoop
      :: StrictTVar m (InboundGovernorState muxMode initiatorCtx peerAddr m a b)
      -> m Void
    inboundGovernorLoop !st = do
      state <- readTVarIO st
      mapTraceWithCache TrInboundGovernorCounters
                        tracer
                        (igsCountersCache state)
                        (inboundGovernorCounters state)
      traceWith tracer $ TrRemoteState $
            mkRemoteSt . csRemoteState
        <$> igsConnections state

      event
        <- atomically $ runFirstToFinish $
               Map.foldMapWithKey
                 (    firstMuxToFinish
                   <> firstMiniProtocolToFinish
                   <> firstPeerPromotedToWarm
                   <> firstPeerPromotedToHot
                   <> firstPeerDemotedToWarm
                   <> firstPeerDemotedToCold
                   <> firstPeerCommitRemote

                   :: EventSignal muxMode initiatorCtx peerAddr versionData m a b
                 )
                 (igsConnections state)
            <> FirstToFinish (
                 NewConnection <$> InfoChannel.readMessage inboundInfoChannel)
      (mbConnId, state') <- case event of
        NewConnection
          -- new connection has been announced by either accept loop or
          -- by connection manager (in which case the connection is in
          -- 'DuplexState').
          (NewConnectionInfo
            provenance
            connId
            csDataFlow
            (Handle csMux muxBundle _ _)) -> do

              traceWith tracer (TrNewConnection provenance connId)
              let responderContext = ResponderContext { rcConnectionId = connId }

              igsConnections <- Map.alterF
                      (\case
                        -- connection
                        Nothing -> do
                          let csMPMHot =
                                [ ( miniProtocolNum mpH
                                  , MiniProtocolData mpH responderContext Hot
                                  )
                                | mpH <- projectBundle SingHot muxBundle
                                ]
                              csMPMWarm =
                                [ ( miniProtocolNum mpW
                                  , MiniProtocolData mpW responderContext Warm
                                  )
                                | mpW <- projectBundle SingWarm muxBundle
                                ]
                              csMPMEstablished =
                                [ ( miniProtocolNum mpE
                                  , MiniProtocolData mpE responderContext Established
                                  )
                                | mpE <- projectBundle SingEstablished muxBundle
                                ]
                              csMiniProtocolMap =
                                  Map.fromList
                                  (csMPMHot ++ csMPMWarm ++ csMPMEstablished)

                          mCompletionMap
                            <-
                            foldM
                              (\acc mpd@MiniProtocolData { mpdMiniProtocol } -> do
                                 result <- runResponder
                                             csMux mpd
                                             Mux.StartOnDemand
                                 case result of
                                   -- synchronous exceptions when starting
                                   -- a mini-protocol are non-recoverable; we
                                   -- close the connection and allow the server
                                   -- to continue.
                                   Left err -> do
                                     traceWith tracer (TrResponderStartFailure connId (miniProtocolNum mpdMiniProtocol) err)
                                     Mux.stopMux csMux
                                     return Nothing

                                   Right completion ->  do
                                     let acc' = Map.insert (miniProtocolNum mpdMiniProtocol)
                                                           completion
                                            <$> acc
                                     -- force under lazy 'Maybe'
                                     case acc' of
                                       Just !_ -> return acc'
                                       Nothing -> return acc'
                              )
                              (Just Map.empty)
                              csMiniProtocolMap

                          case mCompletionMap of
                            -- there was an error when starting one of the
                            -- responders, we let the server continue without this
                            -- connection.
                            Nothing -> return Nothing

                            Just csCompletionMap -> do
                              mv <- traverse registerDelay inboundIdleTimeout
                              let -- initial state is 'RemoteIdle', if the remote end will not
                                  -- start any responders this will unregister the inbound side.
                                  csRemoteState :: RemoteState m
                                  csRemoteState = RemoteIdle (case mv of
                                                                Nothing -> retry
                                                                Just v  -> LazySTM.readTVar v >>= check)

                                  connState = ConnectionState {
                                      csMux,
                                      csDataFlow,
                                      csMiniProtocolMap,
                                      csCompletionMap,
                                      csRemoteState
                                    }

                              return (Just connState)

                        -- inbound governor might be notified about a connection
                        -- which is already tracked.  In such case we preserve its
                        -- state.
                        --
                        -- In particular we preserve an ongoing timeout on
                        -- 'RemoteIdle' state.
                        Just connState -> return (Just connState)

                      )
                      connId
                      (igsConnections state)


              -- update state and continue the recursive loop
              let state' = state { igsConnections }
              return (Just connId, state')

        MuxFinished connId merr -> do

          case merr of
            Nothing  -> traceWith tracer (TrMuxCleanExit connId)
            Just err -> traceWith tracer (TrMuxErrored connId err)

          -- the connection manager does should realise this on itself.
          let state' = unregisterConnection connId state
          return (Just connId, state')

        MiniProtocolTerminated
          Terminated {
              tConnId,
              tMux,
              tMiniProtocolData = mpd@MiniProtocolData { mpdMiniProtocol = miniProtocol },
              tResult
            } ->
          let num = miniProtocolNum miniProtocol in
          case tResult of
            Left e -> do
              -- a mini-protocol errored.  In this case mux will shutdown, and
              -- the connection manager will tear down the socket.  We can just
              -- forget the connection from 'InboundGovernorState'.
              traceWith tracer $
                TrResponderErrored tConnId num e

              let state' = unregisterConnection tConnId state
              return (Just tConnId, state')

            Right _ -> do
              result
                <- runResponder tMux mpd Mux.StartOnDemand
              case result of
                Right completionAction -> do
                  traceWith tracer (TrResponderRestarted tConnId num)
                  let state' = updateMiniProtocol tConnId num completionAction state
                  return (Nothing, state')

                Left err -> do
                  -- there is no way to recover from synchronous exceptions; we
                  -- stop mux which allows to close resources held by
                  -- connection manager.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stopMux tMux

                  let state' = unregisterConnection tConnId state

                  return (Just tConnId, state')


        WaitIdleRemote connId -> do
          -- @
          --    DemotedToCold^{dataFlow}_{Remote} : InboundState Duplex
          --                                      → InboundIdleState Duplex
          -- @
          res <- demotedToColdRemote connectionManager
                                     (remoteAddress connId)
          traceWith tracer (TrWaitIdleRemote connId res)
          case res of
            TerminatedConnection {} -> do
              let state' = unregisterConnection connId state
              return (Just connId, state')
            OperationSuccess {}  -> do
              mv <- traverse registerDelay inboundIdleTimeout
              let timeoutSTM :: STM m ()
                  !timeoutSTM = case mv of
                    Nothing -> retry
                    Just v  -> LazySTM.readTVar v >>= check

              let state' = updateRemoteState connId (RemoteIdle timeoutSTM) state

              return (Just connId, state')
            UnsupportedState {} ->
              return (Just connId, state)

        -- @
        --    PromotedToWarm^{Duplex}_{Remote}
        -- @
        -- or
        -- @
        --    Awake^{dataFlow}_{Remote}
        -- @
        --
        -- Note: the 'AwakeRemote' is detected as soon as mux detects any
        -- traffic.  This means that we'll observe this transition also if the
        -- first message that arrives is terminating a mini-protocol.
        AwakeRemote connId -> do
          -- notify the connection manager about the transition
          res <- promotedToWarmRemote connectionManager
                                      (remoteAddress connId)
          traceWith tracer (TrPromotedToWarmRemote connId res)

          when (resultInState res == UnknownConnectionSt) $ do
            traceWith tracer (TrUnexpectedlyFalseAssertion
                                (InboundGovernorLoop
                                  (Just connId)
                                  UnknownConnectionSt)
                             )
            _ <- evaluate (assert False)
            pure ()

          let state' = updateRemoteState
                         connId
                         RemoteWarm
                         state

          return (Just connId, state')

        RemotePromotedToHot connId -> do
          traceWith tracer (TrPromotedToHotRemote connId)
          let state' = updateRemoteState connId RemoteHot state

          return (Just connId, state')

        RemoteDemotedToWarm connId -> do
          traceWith tracer (TrDemotedToWarmRemote connId)
          let state' = updateRemoteState connId RemoteWarm state

          return (Just connId, state')

        CommitRemote connId -> do
          res <- unregisterInboundConnection connectionManager
                                             (remoteAddress connId)
          traceWith tracer $ TrDemotedToColdRemote connId res
          case res of
            UnsupportedState {} -> do
              -- 'inState' can be either:
              -- @'UnknownConnection'@,
              -- @'InReservedOutboundState'@,
              -- @'InUnnegotiatedState',
              -- @'InOutboundState' 'Unidirectional'@,
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              let state' = unregisterConnection connId state
              return (Just connId, state')

            TerminatedConnection {} -> do
              -- 'inState' can be either:
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              let state' = unregisterConnection connId state
              return (Just connId, state')

            OperationSuccess transition ->
              case transition of
                -- the following two cases are when the connection was not used
                -- by p2p-governor, the connection will be closed.
                CommitTr -> do
                  -- @
                  --    Commit^{dataFlow}_{Remote} : InboundIdleState dataFlow
                  --                               → TerminatingState
                  -- @
                  let state' = unregisterConnection connId state
                  return (Just connId, state')

                -- the connection is still used by p2p-governor, carry on but put
                -- it in 'RemoteCold' state.  This will ensure we keep ready to
                -- serve the peer.
                -- @
                --    DemotedToCold^{Duplex}_{Remote} : DuplexState
                --                                    → OutboundState Duplex
                -- @
                -- or
                -- @
                --    Awake^{Duplex}^{Local} : InboundIdleState Duplex
                --                           → OutboundState Duplex
                -- @
                --
                -- note: the latter transition is level triggered rather than
                -- edge triggered. The server state is updated once protocol
                -- idleness expires rather than as soon as the connection
                -- manager was requested outbound connection.
                KeepTr -> do
                  let state' = updateRemoteState connId RemoteCold state

                  return (Just connId, state')

      mask_ $ do
        atomically $ writeTVar st state'
        case mbConnId of
          Just cid -> traceWith trTracer (mkRemoteTransitionTrace cid state state')
          Nothing  -> pure ()

      inboundGovernorLoop st


-- | Run a responder mini-protocol.
--
-- @'HasResponder' mode ~ True@ is used to rule out
-- 'InitiatorProtocolOnly' case.
--
runResponder :: forall (mode :: MuxMode) initiatorCtx peerAddr m a b.
                 ( Alternative (STM m)
                 , HasResponder mode ~ True
                 , MonadAsync m
                 , MonadCatch m
                 , MonadThrow (STM m)
                 )
              => Mux.Mux mode m
              -> MiniProtocolData mode initiatorCtx peerAddr m a b
              -> Mux.StartOnDemandOrEagerly
              -> m (Either SomeException (STM m (Either SomeException b)))
runResponder mux
              MiniProtocolData {
                  mpdMiniProtocol     = miniProtocol,
                  mpdResponderContext = responderContext
                }
             startStrategy =
    -- do not catch asynchronous exceptions, which are non recoverable
    tryJust (\e -> case fromException e of
              Just (SomeAsyncException _) -> Nothing
              Nothing                     -> Just e) $
      case miniProtocolRun miniProtocol of
        ResponderProtocolOnly responder ->
          Mux.runMiniProtocol
            mux (miniProtocolNum miniProtocol)
            Mux.ResponderDirectionOnly
            startStrategy
            (runMiniProtocolCb responder responderContext)

        InitiatorAndResponderProtocol _ responder ->
          Mux.runMiniProtocol
            mux (miniProtocolNum miniProtocol)
            Mux.ResponderDirection
            startStrategy
            (runMiniProtocolCb responder responderContext)

--
-- Trace
--

-- | Remote connection state tracked by inbound protocol governor.
--
data RemoteSt = RemoteWarmSt
              | RemoteHotSt
              | RemoteIdleSt
              | RemoteColdSt
  deriving (Eq, Show)


mkRemoteSt :: RemoteState m -> RemoteSt
mkRemoteSt  RemoteWarm    = RemoteWarmSt
mkRemoteSt  RemoteHot     = RemoteHotSt
mkRemoteSt (RemoteIdle _) = RemoteIdleSt
mkRemoteSt  RemoteCold    = RemoteColdSt


-- | 'Nothing' represents uninitialised state.
--
type RemoteTransition = Transition' (Maybe RemoteSt)

type RemoteTransitionTrace peerAddr = TransitionTrace' peerAddr (Maybe RemoteSt)

mkRemoteTransitionTrace :: Ord peerAddr
                        => ConnectionId peerAddr
                        -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
                        -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
                        -> RemoteTransitionTrace peerAddr
mkRemoteTransitionTrace connId fromState toState =
    TransitionTrace
      (remoteAddress connId)
      Transition { fromState = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (igsConnections fromState)
                 , toState   = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (igsConnections toState)
                 }


data IGAssertionLocation peerAddr
  = InboundGovernorLoop !(Maybe (ConnectionId peerAddr)) !AbstractState
  deriving Show

data InboundGovernorTrace peerAddr
    = TrNewConnection                !Provenance !(ConnectionId peerAddr)
    | TrResponderRestarted           !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderStartFailure        !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderErrored             !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderStarted             !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderTerminated          !(ConnectionId peerAddr) !MiniProtocolNum
    | TrPromotedToWarmRemote         !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrPromotedToHotRemote          !(ConnectionId peerAddr)
    | TrDemotedToWarmRemote          !(ConnectionId peerAddr)
    | TrDemotedToColdRemote          !(ConnectionId peerAddr) !(OperationResult DemotedToColdRemoteTr)
    -- ^ All mini-protocols terminated.  The boolean is true if this connection
    -- was not used by p2p-governor, and thus the connection will be terminated.
    | TrWaitIdleRemote               !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrMuxCleanExit                 !(ConnectionId peerAddr)
    | TrMuxErrored                   !(ConnectionId peerAddr) SomeException
    | TrInboundGovernorCounters      !InboundGovernorCounters
    | TrRemoteState                  !(Map (ConnectionId peerAddr) RemoteSt)
    | TrUnexpectedlyFalseAssertion   !(IGAssertionLocation peerAddr)
    -- ^ This case is unexpected at call site.
    | TrInboundGovernorError         !SomeException
  deriving Show
