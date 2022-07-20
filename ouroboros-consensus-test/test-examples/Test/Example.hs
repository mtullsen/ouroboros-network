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
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
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
  -- Q. any reason we need to put into ConsensusConfig?
  
data SP             -- The Simplest Protocol

data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
data SP_IsLeader    = SP_IsLeader    -- Evidence that we /are/ leader


data instance ConsensusConfig SP =
  SP_Config { cfgsp_iLeadInSlots :: Set SlotNo }
  deriving NoThunks via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP)

instance ConsensusProtocol SP where
  type ChainDepState SP = ()
  type IsLeader      SP = SP_IsLeader
  type CanBeLeader   SP = SP_CanBeLeader
  type SelectView    SP = BlockNo
  type LedgerView    SP = ()
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


---- Trivial Block (for the SP protocol) -------------------------------------
-- see 4.3 in [CCASL]
--
-- borrowing from
--  - https://iohk.io/en/blog/posts/2020/05/28/the-abstract-nature-of-the-consensus-layer/
--      which references 'MiniConsensus.hs'
--  - ouroboros-consensus-test/test-consensus/Test/Consensus/HardFork/Combinator/A.hs

data TrivBlock =
    TrivBlock
      { bbSignature :: Header TrivBlock
      , bbEpoch     :: EpochNo
      -- , bbRelSlot   :: RelSlot
      }
  deriving NoThunks via OnlyCheckWhnfNamed "TrivBlock" TrivBlock

  -- TODO: change name, this defn. too simple for Byron!

type instance BlockProtocol TrivBlock = SP


-- BlockSupportsProtocol has *many* superclasses!


instance BlockSupportsProtocol TrivBlock where
  validateView = stub -- const bbSignature
  selectView   = stub -- bbSlotNo  -- has default (to ?)


data instance Header TrivBlock = Signature
  deriving NoThunks via OnlyCheckWhnfNamed "TrivBlock" (Header TrivBlock)

instance GetHeader TrivBlock where
  getHeader          = bbSignature
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash TrivBlock where
  headerPrevHash = stub -- hdrB_prev

instance HasHeader TrivBlock where
  getHeaderFields = stub 
                    -- see doc in *.Block.Abstract ; ~complex
                    -- getBlockHeaderFields - seems to rely on a bit more
                    
instance StandardHash TrivBlock
  -- ?!
  
type instance HeaderHash  TrivBlock = String -- Strict.ByteString

instance HasHeader (Header TrivBlock) where
  getHeaderFields _b = stub -- :: HeaderFields (Header TrivBlock)
                    -- castHeaderFields . bbSignature

data instance BlockConfig TrivBlock = BCfgTrivBlock
  deriving (Generic, NoThunks)

data instance CodecConfig TrivBlock = CCfgTrivBlock
  deriving (Generic, NoThunks)

data instance StorageConfig TrivBlock = SCfgTrivBlock
  deriving (Generic, NoThunks)


---- library -----------------------------------------------------------------

stub :: a
stub = error "stub"
