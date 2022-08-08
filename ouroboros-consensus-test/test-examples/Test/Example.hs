{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Test.Example where

-- base pkgs:
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           GHC.Generics (Generic)

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

---- Define Protocol B : 'PrtclB' --------------------------------------------

data PrtclB             -- TODO:_

data PrtclB_CanBeLeader = PrtclB_CanBeLeader -- Evidence we /can/ be a leader
data PrtclB_IsLeader    = PrtclB_IsLeader    -- Evidence we /are/ leader

data instance ConsensusConfig PrtclB =
  PrtclB_Config { cfgsp_iLeadInSlots  :: Set SlotNo
                , cfgsp_securityParam :: SecurityParam
                }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclB_Config"
                        (ConsensusConfig PrtclB)


instance ConsensusProtocol PrtclB where
  
  type ChainDepState PrtclB = ()
  type IsLeader      PrtclB = PrtclB_IsLeader
  type CanBeLeader   PrtclB = PrtclB_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    PrtclB = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    PrtclB = ()
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclB = ()              
  
  type ValidationErr PrtclB = Void

  checkIsLeader cfg PrtclB_CanBeLeader slot _tcds =
      if slot `Set.member` cfgsp_iLeadInSlots cfg
      then Just PrtclB_IsLeader
      else Nothing

  protocolSecurityParam = cfgsp_securityParam

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- works b/c ChainDepState PrtclB = ()
                                  
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()


sp_config :: ConsensusConfig PrtclB
sp_config = PrtclB_Config
              { cfgsp_iLeadInSlots = Set.empty -- never a leader, FIXME
              , cfgsp_securityParam= SecurityParam{maxRollbacks= 1}
              }

---- Block B (for Protocol B) ------------------------------------------------

-- | Map the block to the consensus protocol PrtclB
type instance BlockProtocol BlockB = PrtclB

-- | Define BlockB
data BlockB = BlockB { bb_header :: Header BlockB
                     }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockB" BlockB

  -- BlockB has no tx's, nothing but a header

-- | And 
instance BlockSupportsProtocol BlockB where
  validateView _ _ = ()
  -- selectView   = stub
  -- this method defaulted.  (MT-TODO: understand.)
  -- TODO: do we want to use the defalt method in some/all of our pills?

-- | the minimum header:
data instance Header BlockB =
  HdrBlockB
    { htb_SlotNo  :: SlotNo
    , htb_BlockNo :: BlockNo
    , htb_Hash    :: HeaderHash BlockB
    , htb_prev    :: ChainHash BlockB
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockB" (Header BlockB)

instance GetHeader BlockB where
  getHeader          = bb_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash BlockB where
  headerPrevHash = htb_prev

-- | one might like "HasHeaderData" as a better name??
instance HasHeader (Header BlockB) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = htb_SlotNo hdr
                          , headerFieldBlockNo= htb_BlockNo hdr
                          , headerFieldHash   = htb_Hash hdr
                          }

instance HasHeader BlockB where
  getHeaderFields = castHeaderFields       -- worth some commentary?
                  . getHeaderFields
                  . bb_header
                    
type instance HeaderHash BlockB = Hash

instance StandardHash BlockB
  
data instance BlockConfig BlockB = BCfgBlockB
  deriving (Generic, NoThunks)
data instance CodecConfig BlockB = CCfgBlockB
  deriving (Generic, NoThunks)
data instance StorageConfig BlockB = SCfgBlockB
  deriving (Generic, NoThunks)


---- A Simple Ledger B -------------------------------------------------------

data instance LedgerState BlockB =
  LedgerB
    { lb_tip :: Point BlockB -- (header hash and slot num)
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerB" (LedgerState BlockB)

---- data --------------------------------------------------------------------

blockB :: BlockB
blockB = BlockB { bb_header= HdrBlockB stub stub stub stub -- TODO
                }


---- pill C ------------------------------------------------------------------
-- TODO: put into separate module.

data BlockC = BlockC
      -- , bc_body   :: [GenTx BlockB]
      -- , bc_Epoch  :: EpochNo

data instance GenTx BlockC = TxC { txName :: String }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxC" (GenTx BlockC)

blockC = BlockC
          -- , tb_body  = [TxA "tx1", TxA "tx2"]

