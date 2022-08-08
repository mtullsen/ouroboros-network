{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Test.ToyLedgerC where

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


---- Define Protocol C : 'PrtclC' --------------------------------------------

data PrtclC         

data PrtclC_CanBeLeader = PrtclC_CanBeLeader -- Evidence we /can/ be a leader
data PrtclC_IsLeader    = PrtclC_IsLeader    -- Evidence we /are/ leader

data instance ConsensusConfig PrtclC =
  PrtclC_Config { cfgsp_iLeadInSlots  :: Set SlotNo
                , cfgsp_securityParam :: SecurityParam
                }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclC_Config"
                        (ConsensusConfig PrtclC)


instance ConsensusProtocol PrtclC where
  
  type ChainDepState PrtclC = ()
  type IsLeader      PrtclC = PrtclC_IsLeader
  type CanBeLeader   PrtclC = PrtclC_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    PrtclC = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    PrtclC = ()               -- FIXME
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclC = ()               -- FIXME
  type ValidationErr PrtclC = Void

  checkIsLeader cfg PrtclC_CanBeLeader slot _tcds =
      if slot `Set.member` cfgsp_iLeadInSlots cfg
      then Just PrtclC_IsLeader
      else Nothing

  protocolSecurityParam = ccpc_securityParam

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- works b/c ChainDepState PrtclC = ()
                                  
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()


pc_config :: ConsensusConfig PrtclC
pc_config = PrtclC_Config
              { ccpc_iLeadInSlots = Set.empty -- never a leader, FIXME
              , ccpc_securityParam= SecurityParam{maxRollbacks= 1}
              }

---- Block C (for Protocol C) ------------------------------------------------

-- | Define BlockC
data BlockC = BlockC { bc_header :: Header BlockC
                     , bc_body   :: [GenTx BlockC]
                     }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockC" BlockC


-- associate BlockC to the consensus protocol PrtclC:

type instance BlockProtocol BlockC = PrtclC

instance BlockSupportsProtocol BlockC where
  validateView _ _ = ()
  -- selectView   = stub
  -- this method defaulted.  (MT-TODO: understand.)
  -- TODO: do we want to use the defalt method in some/all of our pills?

-- define the transaction type for BlockC

data instance GenTx BlockC = TxC { txName :: String }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxC" (GenTx BlockC)


---- Configuration boilerplate: ----------------------------------------------

data instance BlockConfig BlockC = BCfgBlockC
  deriving (Generic, NoThunks)
data instance CodecConfig BlockC = CCfgBlockC
  deriving (Generic, NoThunks)
data instance StorageConfig BlockC = SCfgBlockC
  deriving (Generic, NoThunks)


---- The Ledger State for Block C --------------------------------------------

data instance LedgerState BlockC =
  LedgerC
    { lsc_tip :: Point BlockC -- (header hash and slot num)
    , lsc_x   :: Int          -- just an up/down counter
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerC" (LedgerState BlockC)

---- data --------------------------------------------------------------------

blockC :: BlockC
blockC = BlockC { bc_header= HdrBlockC stub stub stub stub -- TODO
                , bc_body  = []
                }
          -- , tb_body  = -- [TxC "tx1", TxC "tx2"]


---- header miscellanea (skim over?) -----------------------------------------

-- | the minimum header:
data instance Header BlockC =
  HdrBlockC
    { hbc_SlotNo  :: SlotNo
    , hbc_BlockNo :: BlockNo
    , hbc_Hash    :: HeaderHash BlockC
    , hbc_prev    :: ChainHash BlockC
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockC" (Header BlockC)

instance GetHeader BlockC where
  getHeader          = bb_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash BlockC where
  headerPrevHash = hbc_prev

-- | one might like "HasHeaderData" as a better name??
instance HasHeader (Header BlockC) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = hbc_SlotNo hdr
                          , headerFieldBlockNo= hbc_BlockNo hdr
                          , headerFieldHash   = hbc_Hash hdr
                          }

instance HasHeader BlockC where
  getHeaderFields = castHeaderFields       -- worth some commentary?
                  . getHeaderFields
                  . bc_header
                    
type instance HeaderHash BlockC = Hash

instance StandardHash BlockC

