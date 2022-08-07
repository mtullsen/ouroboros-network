{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Test.ToyLedger1 where

-- base pkgs:
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)

-- pkgs tasty*:
-- import           Test.Tasty
-- import           Test.Tasty.QuickCheck

-- pkg nothunks:
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

-- pkg serialise:
import           Codec.Serialise

-- pkg ouroboros-consensus:
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)

-- local modules:
import           Test.Utilities


---- Define Protocol 'SP' - Simplest Protocol --------------------------------

data SP             -- The Simplest Protocol

data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
data SP_IsLeader    = SP_IsLeader    -- Evidence that we /are/ leader

data instance ConsensusConfig SP =
  SP_Config { cfgsp_iLeadInSlots :: Set SlotNo
            }
  deriving NoThunks via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP)


instance ConsensusProtocol SP where
  
  type ChainDepState SP = ()
  type IsLeader      SP = SP_IsLeader
  type CanBeLeader   SP = SP_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    SP = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    SP = ()
  
  -- | View on a block header required for header validation
  type ValidateView  SP = ()              
  
  type ValidationErr SP = Void

  checkIsLeader cfg SP_CanBeLeader slot _tcds =
      if slot `Set.member` cfgsp_iLeadInSlots cfg
      then Just SP_IsLeader
      else Nothing

  protocolSecurityParam _cfg = k

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- works b/c ChainDepState SP = ()
                                  
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()

  -- NF: TODO: look at Shelley instance of ^

k :: SecurityParam
k = SecurityParam {maxRollbacks= 1}

---- Trivial Block -----------------------------------------------------------

-- | Map the block to a consensus protocol
type instance BlockProtocol TrivBlock = SP

-- | Define TrivBlock
data TrivBlock =
    TrivBlock
      { tb_header :: Header TrivBlock
      }
  deriving NoThunks via OnlyCheckWhnfNamed "TrivBlock" TrivBlock

  -- TrivBlock has no tx's.

data instance Header TrivBlock = HdrTB {
      hdrTB_fields :: HeaderFields TrivBlock -- TODO: NF: inline! make simpler.
    , hdrTB_prev   :: ChainHash    TrivBlock
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrTB" (Header TrivBlock)


instance BlockSupportsProtocol TrivBlock where
  validateView _ _ = ()
  -- selectView   = stub  -- method defaulted.  (MT-TODO: understand.)

-- | the two direct super-classes of BlockSupportsProtocol:

instance GetHeader TrivBlock where
  getHeader          = tb_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash TrivBlock where
  headerPrevHash = hdrTB_prev


instance HasHeader TrivBlock where
  getHeaderFields = getBlockHeaderFields
                    -- see doc in *.Block.Abstract ; ~complex
                    -- getBlockHeaderFields - seems to rely on ...

                    
instance HasHeader (Header TrivBlock) where
  getHeaderFields = castHeaderFields . hdrTB_fields
  
instance StandardHash TrivBlock
  
type instance HeaderHash  TrivBlock = String -- Strict.ByteString



data instance BlockConfig TrivBlock = BCfgTrivBlock
  deriving (Generic, NoThunks)

data instance CodecConfig TrivBlock = CCfgTrivBlock
  deriving (Generic, NoThunks)

data instance StorageConfig TrivBlock = SCfgTrivBlock
  deriving (Generic, NoThunks)


---- Now, define a Plain Ledger, A -------------------------------------------

data instance LedgerState TrivBlock = LedgerA {
      lgrA_tip :: Point TrivBlock
      -- (header hash and slot num)
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerA" (LedgerState TrivBlock)

  -- BTW: A&B were testing the transition.


---- data --------------------------------------------------------------------

trivBlock :: TrivBlock
trivBlock = TrivBlock { tb_header= HdrTB stub stub -- TODO
                      }


---- pill 2 ------------------------------------------------------------------
-- TODO: put into separate module.

data Block2 = Block2
      -- , tb_body   :: [GenTx TrivBlock]
      -- , tb_Epoch  :: EpochNo

data instance GenTx Block2 = Tx2 { txName :: String }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "Tx2" (GenTx Block2)

exaBlock2 = Block2
              -- , tb_body  = [TxA "tx1", TxA "tx2"]


