{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Test.Example
  ( test1
  
  -- making exploration easier (no other reason):
  , module Ouroboros.Consensus.Protocol.Abstract
  )
  where

-- base pkgs:
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)

-- pkgs tasty*:
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- pkg ?:
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

-- pkg ouroboros-consensus:
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Config.SecurityParam
                 -- though ...Protocol.Abstract does a convenience re-export

import           Ouroboros.Consensus.Block.Abstract -- (SlotNo)
import           Ouroboros.Consensus.Block.SupportsProtocol

import           Ouroboros.Consensus.Ticked


---- tests -------------------------------------------------------------------
-- TODO: put something interesting in here.

test1 :: TestTree
test1 = testProperty "prop_example1" prop_example1

prop_example1 :: Bool
prop_example1 = True


---- Simplest Protocol 'SP' --------------------------------------------------

k :: SecurityParam
k = SecurityParam {maxRollbacks= 0}
  -- Q. any reason to put into ConsensusConfig?
  
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

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- TODO: huh? explore implem.
                                  -- works b/c ChainDepState SP = ()
                                  
    -- [the doc doesn't make entirely clear]
    
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()

---- Trivial Block (to work with SP) -----------------------------------------
-- see 4.3 in [CCASL]

-- borrowing from 'MiniConsensus.hs' via
-- https://iohk.io/en/blog/posts/2020/05/28/the-abstract-nature-of-the-consensus-layer/

data ByronBlock =
    ByronBlock
      { bbSignature :: Header ByronBlock
      , bbEpoch     :: EpochNo
      -- , bbRelSlot   :: RelSlot
      }
  deriving NoThunks via OnlyCheckWhnfNamed "ByronBlock" ByronBlock

  -- TODO: change name, this defn. too simple for Byron!

type instance BlockProtocol ByronBlock = SP


-- BlockSupportsProtocol has *many* superclasses!


instance BlockSupportsProtocol ByronBlock where
  validateView = stub -- const bbSignature
  -- selectView   = bbSlotNo  -- has default (to ?)


data instance Header ByronBlock = Signature
  deriving NoThunks via OnlyCheckWhnfNamed "ByronBlock" (Header ByronBlock)

instance GetHeader ByronBlock where
  getHeader          = bbSignature
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash ByronBlock where
  headerPrevHash = stub -- hdrB_prev

instance HasHeader ByronBlock where
  getHeaderFields = stub 
                    -- see doc in *.Block.Abstract ; ~complex
                    -- getBlockHeaderFields - seems to rely on a bit more
                    
instance StandardHash ByronBlock

type instance HeaderHash  ByronBlock = String -- Strict.ByteString

instance HasHeader (Header ByronBlock) where
  getHeaderFields _b = stub :: HeaderFields (Header ByronBlock)
                    -- castHeaderFields . bbSignature

data instance BlockConfig ByronBlock = BCfgByronBlock
  deriving (Generic, NoThunks)

data instance CodecConfig ByronBlock = CCfgByronBlock
  deriving (Generic, NoThunks)

data instance StorageConfig ByronBlock = SCfgByronBlock
  deriving (Generic, NoThunks)


---- library -----------------------------------------------------------------

stub = error "stub"
