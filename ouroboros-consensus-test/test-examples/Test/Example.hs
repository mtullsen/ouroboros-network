{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}

module Test.Example
  ( test1
  -- making exploration easier (no other reason)
  , module Ouroboros.Consensus.Protocol.Abstract
  )
  where

-- base pkgs:
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void

-- pkgs tasty*:
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- pkg ouroboros-consensus:
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Config.SecurityParam
                 -- though ...Protocol.Abstract does a convenience re-export

import           Ouroboros.Consensus.Block.Abstract (SlotNo)
import           Ouroboros.Consensus.Ticked
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

---- tests -------------------------------------------------------------------
-- TODO: put something interesting in here.

test1 :: TestTree
test1 = testProperty "prop_example1" prop_example1

prop_example1 :: Bool
prop_example1 = True


---- Simplest Protocol 'SP' --------------------------------------------------

k :: SecurityParam
k = SecurityParam {maxRollbacks= 0}
  -- need to be in ConsensusConfig??
  
data SP             -- The Simplest Protocol

data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
data SP_IsLeader    = SP_IsLeader    -- Evidence that we /are/ leader


data instance ConsensusConfig SP =
  SP_Config { cfgsp_iLeadInSlots :: Set SlotNo }
  deriving NoThunks via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP)
           -- TODO: huh?

instance ConsensusProtocol SP where
  type ChainDepState SP = ()
  type LedgerView    SP = ()
  type IsLeader      SP = SP_IsLeader
  type CanBeLeader   SP = SP_CanBeLeader
  type ValidateView  SP = ()
  type ValidationErr SP = Void

  checkIsLeader cfg SP_CanBeLeader slot _tcds =
      if slot `Set.member` cfgsp_iLeadInSlots cfg
      then Just SP_IsLeader
      else Nothing

  protocolSecurityParam _cfg = k

  tickChainDepState     _ _ _ _ = TickedTrivial -- TODO:huh?
  
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()

