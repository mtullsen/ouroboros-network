{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TupleSections     #-}

module Ouroboros.Network.Testing.Data.Script
  ( -- * Test scripts
    Script (..)
  , NonEmpty (..)
  , scriptHead
  , singletonScript
  , initScript
  , stepScript
  , stepScriptSTM
  , stepScriptOrFinish
  , stepScriptOrFinishSTM
  , initScript'
  , stepScript'
  , stepScriptSTM'
  , shrinkScriptWith
  , arbitraryScriptOf
  , prop_shrink_Script
    -- * Timed scripts
  , ScriptDelay (..)
  , TimedScript
  , singletonTimedScript
  , playTimedScript
    -- * Pick scripts
  , PickScript
  , PickMembers (..)
  , arbitraryPickScript
  , interpretPickScript
  ) where

import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Concurrent.Class.MonadSTM
import           Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Testing.Utils (prop_shrink_nonequal,
                     shrinkVector)
import           Test.QuickCheck

--
-- Test script abstraction
--

newtype Script a = Script (NonEmpty a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

singletonScript :: a -> Script a
singletonScript x = Script (x :| [])

scriptHead :: Script a -> a
scriptHead (Script (x :| _)) = x

arbitraryScriptOf :: Int -> Gen a -> Gen (Script a)
arbitraryScriptOf maxSz a =
    sized $ \sz -> do
      n <- choose (1, max 1 (min maxSz sz))
      Script . NonEmpty.fromList <$> vectorOf n a

initScript :: MonadSTM m
            => Script a
            -> STM m (TVar m (Script a))
initScript = LazySTM.newTVar

stepScript :: MonadSTM m => TVar m (Script a) -> m a
stepScript scriptVar = atomically (stepScriptSTM scriptVar)

stepScriptSTM :: MonadSTM m => TVar m (Script a) -> STM m a
stepScriptSTM scriptVar = do
    Script (x :| xs) <- LazySTM.readTVar scriptVar
    case xs of
      []     -> return ()
      x':xs' -> LazySTM.writeTVar scriptVar (Script (x' :| xs'))
    return x

-- | Return 'Left' if it was the last step, return 'Right' if the script can
-- continue.
--
stepScriptOrFinish :: MonadSTM m => TVar m (Script a) -> m (Either a a)
stepScriptOrFinish scriptVar = atomically (stepScriptOrFinishSTM scriptVar)

stepScriptOrFinishSTM :: MonadSTM m => TVar m (Script a) -> STM m (Either a a)
stepScriptOrFinishSTM scriptVar = do
    Script (x :| xs) <- LazySTM.readTVar scriptVar
    case xs of
      []     -> return (Left x)
      x':xs' -> writeTVar scriptVar (Script (x' :| xs'))
             $> Right x

initScript' :: MonadSTM m => Script a -> m (TVar m (Script a))
initScript' = newTVarIO

stepScript' :: MonadSTM m => TVar m (Script a) -> m a
stepScript' scriptVar = atomically (stepScriptSTM scriptVar)

stepScriptSTM' :: MonadSTM m => TVar m (Script a) -> STM m a
stepScriptSTM' scriptVar = do
    Script (x :| xs) <- readTVar scriptVar
    case xs of
      []     -> return ()
      x':xs' -> writeTVar scriptVar (Script (x' :| xs'))
    return x

instance Arbitrary a => Arbitrary (Script a) where
    arbitrary = sized $ \sz -> arbitraryScriptOf sz arbitrary
    shrink = shrinkScriptWith shrink

shrinkScriptWith :: (a -> [a]) -> Script a -> [Script a]
shrinkScriptWith f (Script (x :| [])) = [ Script (x' :| []) | x' <- f x ]
shrinkScriptWith f (Script (x :| xs)) =
        Script (x :| [])                          -- drop whole tail
      : Script (x :| take (length xs `div` 2) xs) -- drop half the tail
      : Script (x :| init xs)                     -- drop only last

        -- drop none, shrink only elements
      : [ Script (x' :| xs) | x'  <- f x ]
     ++ [ Script (x :| xs') | xs' <- shrinkVector f xs ]


--
-- Timed scripts
--

type TimedScript a = Script (a, ScriptDelay)

-- | Timed script which consists of a single element.
--
singletonTimedScript :: a -> TimedScript a
singletonTimedScript = singletonScript . (,NoDelay)

data ScriptDelay = NoDelay | ShortDelay | LongDelay | Delay DiffTime
  deriving (Eq, Show)

instance Arbitrary ScriptDelay where
  arbitrary = frequency [ (1, pure NoDelay)
                        , (1, pure ShortDelay)
                        , (4, pure LongDelay) ]

  shrink LongDelay  = [NoDelay, ShortDelay]
  shrink ShortDelay = [NoDelay]
  shrink NoDelay    = []
  shrink (Delay _)  = []

playTimedScript :: (MonadAsync m, MonadDelay m)
                => Tracer m a -> TimedScript a -> m (TVar m a)
playTimedScript tracer (Script ((x0,d0) :| script)) = do
    v <- newTVarIO x0
    traceWith tracer x0
    _ <- async $ do
           labelThisThread "timed-script"
           threadDelay (interpretScriptDelay d0)
           sequence_ [ do atomically (writeTVar v x)
                          traceWith tracer x
                          threadDelay (interpretScriptDelay d)
                     | (x,d) <- script ]
    return v
  where
    interpretScriptDelay NoDelay       = 0
    interpretScriptDelay ShortDelay    = 1
    interpretScriptDelay LongDelay     = 3600
    interpretScriptDelay (Delay delay) = delay


--
-- Pick scripts
--

-- | A pick script is used to interpret the 'policyPickKnownPeersForPeerShare' and
-- the 'policyPickColdPeersToForget'. It selects elements from the given
-- choices by their index (modulo the number of choices). This representation
-- was chosen because it allows easy shrinking.
--
type PickScript peeraddr = Script (PickMembers peeraddr)

data PickMembers peeraddr = PickFirst
                          | PickAll
                          | PickSome (Set peeraddr)
  deriving (Eq, Show)

instance (Arbitrary peeraddr, Ord peeraddr) =>
         Arbitrary (PickMembers peeraddr) where
    arbitrary = arbitraryPickMembers (Set.fromList <$> listOf1 arbitrary)

    shrink (PickSome ixs) = PickFirst
                          : PickAll
                          : [ PickSome ixs'
                            | ixs' <- shrink ixs
                            , not (Set.null ixs') ]
    shrink PickAll        = [PickFirst]
    shrink PickFirst      = []

arbitraryPickMembers :: Gen (Set peeraddr) -> Gen (PickMembers peeraddr)
arbitraryPickMembers pickSome =
    frequency [ (1, pure PickFirst)
              , (1, pure PickAll)
              , (2, PickSome <$> pickSome)
              ]

arbitraryPickScript :: Gen (Set peeraddr) -> Gen (PickScript peeraddr)
arbitraryPickScript pickSome =
    sized $ \sz ->
      arbitraryScriptOf sz (arbitraryPickMembers pickSome)

interpretPickScript :: (MonadSTM m, Ord peeraddr)
                    => TVar m (PickScript peeraddr)
                    -> Set peeraddr
                    -> Int
                    -> STM m (Set peeraddr)
interpretPickScript scriptVar available pickNum
  | Set.null available
  = error "interpretPickScript: given empty map to pick from"
  | pickNum <= 0
  = error "interpretPickScript: given invalid pickNum"

  | otherwise
  = do pickmembers <- stepScriptSTM scriptVar
       return (interpretPickMembers pickmembers available pickNum)

interpretPickMembers :: Ord peeraddr
                     => PickMembers peeraddr
                     -> Set peeraddr -> Int -> Set peeraddr
interpretPickMembers PickFirst     ps _ = Set.singleton (Set.elemAt 0 ps)
interpretPickMembers PickAll       ps n = Set.take n ps
interpretPickMembers (PickSome as) ps n
  | Set.null ps' = Set.singleton (Set.elemAt 0 ps)
  | otherwise    = Set.take n ps'
  where
    ps' = Set.intersection ps as


--
-- Tests for the QC Arbitrary instances
--

prop_shrink_Script :: Fixed (Script Int) -> Property
prop_shrink_Script = prop_shrink_nonequal

