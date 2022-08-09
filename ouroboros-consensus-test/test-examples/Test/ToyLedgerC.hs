{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Test.ToyLedgerC where

-- base pkgs:
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word (Word64)
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
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Forecast (trivialForecast)

-- local modules:
import           Test.Utilities


---- Define Protocol C : 'PrtclC' --------------------------------------------

data PrtclC         

data PrtclC_CanBeLeader = PrtclC_CanBeLeader -- Evidence we /can/ be a leader
data PrtclC_IsLeader    = PrtclC_IsLeader    -- Evidence we /are/ leader

data instance ConsensusConfig PrtclC =
  PrtclC_Config { ccpc_iLeadInSlots  :: Set SlotNo
                , ccpc_securityParam :: SecurityParam  -- i.e., 'k'
                }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclC_Config"
                        (ConsensusConfig PrtclC)

instance ConsensusProtocol PrtclC where
  
  type ChainDepState PrtclC = ()   -- FIXME: want for C
  type IsLeader      PrtclC = PrtclC_IsLeader
  type CanBeLeader   PrtclC = PrtclC_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    PrtclC = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    PrtclC = ()               -- TODO-D.
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclC = ()               -- TODO-D
  type ValidationErr PrtclC = Void

  checkIsLeader cfg PrtclC_CanBeLeader slot _tcds =
      if slot `Set.member` ccpc_iLeadInSlots cfg
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
    -- Q. do we want to use the default method in some/all of our pills?


-- The transaction type for BlockC

data Tx = Inc | Dec
  deriving (Show, Eq, Generic, Serialise)

data instance GenTx BlockC = TxC Tx
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxC" (GenTx BlockC)

-- And what it means for the transaction to be Validated (trivial for now)

data instance Validated (GenTx BlockC) = ValidatedTxC
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks


---- The Ledger State for Block C --------------------------------------------

data instance LedgerState BlockC =
  LedgerC
    { lsbc_tip   :: Point BlockC -- header hash and slot num.
    , lsbc_count :: Word64       -- results of an up/down counter
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerC" (LedgerState BlockC)

newtype instance Ticked (LedgerState BlockC) =
  TickedLedgerStateC {
    unTickedLedgerStateC :: LedgerState BlockC
  }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks

-- No LedgerCfg data:
type instance LedgerCfg (LedgerState BlockC) = ()

type instance ApplyTxErr BlockC = ()


---- Now for some class instances --------------------------------------------

instance GetTip (Ticked(LedgerState BlockC)) where
  getTip = stub lsbc_tip

instance GetTip (LedgerState BlockC) where
  getTip = stub lsbc_tip

-- FIXME: appears you need both the above: both need more "glue"

type LedgerErr_BlockC = String
  
instance IsLedger (LedgerState BlockC) where
  type instance LedgerErr      (LedgerState BlockC) = LedgerErr_BlockC 
  type instance AuxLedgerEvent (LedgerState BlockC) = ()

  applyChainTickLedgerResult _cfg slot ldgrSt =
    LedgerResult {lrEvents= [], lrResult= tickLedgerStateC slot ldgrSt}

    -- updating the slot, but otherwise no ledger changes. (?)

instance ApplyBlock (LedgerState BlockC) BlockC where
  applyBlockLedgerResult ldgrCfg b tickedLdgrSt =
    return $
      LedgerResult { lrEvents= []
                   , lrResult= stub -- yada, yada
                                -- (applyTx ldgrCfg stub slot)
                                -- tickedLdgrSt
                                -- (bc_body b)
                                -- {lsbc_tip= stub} -- TODO
                   } 
      where
      slot = stub -- TODO

  reapplyBlockLedgerResult _lc b tl =
    LedgerResult {lrEvents= [], lrResult= stub b}          -- FIXME

tickLedgerStateC ::
  SlotNo -> LedgerState BlockC -> Ticked (LedgerState BlockC)
tickLedgerStateC slot ldgrSt =
  -- just update the slot
  --   and just leave the hash unchanged?  Q. Nick?
  TickedLedgerStateC ldgrSt{lsbc_tip=BlockPoint slot hash'}

  where
  hash' = stub       -- FIXME (getting into the weeds here to get working)
        $ pointHash  -- this might be origin
        $ lsbc_tip ldgrSt
  
        -- See Ouroboros.Network.Block for useful functions
  
-- | no methods here, 'UpdateLedger' is a class to roll-up classes:
instance UpdateLedger BlockC where {}  

instance LedgerSupportsProtocol BlockC where
  protocolLedgerView _lc tl = TickedTrivial 
  ledgerViewForecastAt = stub
                          -- FIXME ^, use trivialForecast?
    -- For PrtclD: want this to be more.
    
instance LedgerSupportsMempool BlockC where
  
  txInvariant _tx = True   -- same as default method

  applyTx _lc _ slotno tx (TickedLedgerStateC ldgrSt) =
    return ( TickedLedgerStateC 
               ldgrSt{lsbc_count= applyTxC tx (lsbc_count ldgrSt)}
           , ValidatedTxC
           )
    where
    applyTxC (TxC Inc) i = i+1
    applyTxC (TxC Dec) i = i-1
    
  -- TODO: many more methods here.


---- Let's just ignore these for now -----------------------------------------

instance HasAnnTip               BlockC where {}
instance ValidateEnvelope        BlockC where {}
instance BasicEnvelopeValidation BlockC where {}


---- Data --------------------------------------------------------------------

blockC :: BlockC
blockC = BlockC { bc_header= HdrBlockC stub stub stub stub -- TODO
                , bc_body  = [TxC Inc, TxC Inc]
                }



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
  getHeader          = bc_header
  blockMatchesHeader = \_ _ -> True -- FIXME: be able to fail
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


---- Configuration boilerplate: ----------------------------------------------

data instance BlockConfig BlockC = BCfgBlockC
  deriving (Generic, NoThunks)
data instance CodecConfig BlockC = CCfgBlockC
  deriving (Generic, NoThunks)
data instance StorageConfig BlockC = SCfgBlockC
  deriving (Generic, NoThunks)



