{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Test.ToyLedgerD where

-- base pkgs:
import           Control.Monad
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


---- Define Protocol D : 'PrtclD' --------------------------------------------

data PrtclD         

data PrtclD_CanBeLeader = PrtclD_CanBeLeader -- Evidence we /can/ be a leader
data PrtclD_IsLeader    = PrtclD_IsLeader    -- Evidence we /are/ leader

data instance ConsensusConfig PrtclD =
  PrtclD_Config { ccpc_iLeadInSlots  :: Set SlotNo
                , ccpc_securityParam :: SecurityParam  -- i.e., 'k'
                }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclD_Config"
                        (ConsensusConfig PrtclD)

instance ConsensusProtocol PrtclD where
  
  type ChainDepState PrtclD = ()   -- TODO: want more here, for C? for D?
  type IsLeader      PrtclD = PrtclD_IsLeader
  type CanBeLeader   PrtclD = PrtclD_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    PrtclD = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    PrtclD = ()               -- TODO: change for Prtcl D.
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclD = ()               -- TODO: change for Prtcl D
  type ValidationErr PrtclD = Void

  checkIsLeader cfg PrtclD_CanBeLeader slot _tcds =
      if slot `Set.member` ccpc_iLeadInSlots cfg
      then Just PrtclD_IsLeader
      else Nothing

  protocolSecurityParam = ccpc_securityParam

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- works b/c ChainDepState PrtclD = ()

  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()


pc_config :: ConsensusConfig PrtclD
pc_config = PrtclD_Config
              { ccpc_iLeadInSlots = Set.empty -- NOTE: never a leader.
              , ccpc_securityParam= SecurityParam{maxRollbacks= 1}
              }


---- Block D (for Protocol D) ------------------------------------------------

-- | Define BlockD
data BlockD = BlockD { bc_header :: Header BlockD
                     , bc_body   :: [GenTx BlockD]
                     }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockD" BlockD


-- associate BlockD to the protocol PrtclD:

type instance BlockProtocol BlockD = PrtclD

instance BlockSupportsProtocol BlockD where
  validateView _bcfg _hdr = ()
  selectView   _bcfg hdr  = blockNo hdr
                            -- i.e., the default method.
                            -- i.e., we only need the block no in order
                            -- to do chain selection
                            -- see -.Protocol.Abstract.preferCandidate
  

-- The transaction type for BlockD:

data Tx = Inc | Dec
  deriving (Show, Eq, Generic, Serialise)

data instance GenTx BlockD = TxD Tx
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxC" (GenTx BlockD)

-- And what it means for the transaction to be Validated (trivial for now)
-- - Note that Validated must include the transaction as well as the evidence

data instance Validated (GenTx BlockD) = ValidatedTxD (GenTx BlockD)
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks


---- The Ledger State for Block D: Type family instances ---------------------

data instance LedgerState BlockD =
  LedgerC
    { lsbc_tip   :: Point BlockD
                                 -- Point for the last applied block.
                                 --  (Point is header hash and slot num)
    , lsbc_count :: Word64       -- results of an up/down counter
    }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

newtype instance Ticked (LedgerState BlockD) =
  TickedLedgerStateD {
    unTickedLedgerStateD :: LedgerState BlockD
  }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- No LedgerCfg data:
type instance LedgerCfg (LedgerState BlockD) = ()

type LedgerErr_BlockD = String
  
type instance ApplyTxErr BlockD = LedgerErr_BlockD


---- The Ledger State for Block D: class instances ---------------------------

instance GetTip (Ticked (LedgerState BlockD)) where
  getTip = castPoint . lsbc_tip . unTickedLedgerStateD

instance GetTip (LedgerState BlockD) where
  getTip = castPoint . lsbc_tip


instance IsLedger (LedgerState BlockD) where
  type instance LedgerErr      (LedgerState BlockD) = LedgerErr_BlockD 
  type instance AuxLedgerEvent (LedgerState BlockD) = ()

  -- | This method is for updating the ledger state when it needs to change
  -- based on time (slot) changing.  In this case, nothing needs to be done.
  -- 
  -- This method shall not update the tip.
  -- Note the doc for the class:
  -- 
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st

  applyChainTickLedgerResult _cfg slot ldgrSt =
    LedgerResult {lrEvents= [], lrResult= tickLedgerStateD slot ldgrSt}

instance ApplyBlock (LedgerState BlockD) BlockD where
  applyBlockLedgerResult ldgrCfg b tickedLdgrSt =
    do
    TickedLedgerStateD ldgrSt <-
      foldM 
        (\tls tx-> fst <$> applyTx ldgrCfg DoNotIntervene slot tx tls)
        tickedLdgrSt
        (bc_body b)
      
    return $
      LedgerResult { lrEvents= []
                   , lrResult= ldgrSt
                   } 
    where
    slot = blockSlot (getHeader b)

  reapplyBlockLedgerResult _lc b _tl =
    LedgerResult {lrEvents= [], lrResult= stub b}
    -- TODO: fill in; though this would be boilerplate.

-- | tickLedgerStateD - helper function to tick the LedgerState
--     currently this is effectively a NOP.  [TODO for Protocol D!]
tickLedgerStateD ::
  SlotNo -> LedgerState BlockD -> Ticked (LedgerState BlockD)
tickLedgerStateD _slot ldgrSt = TickedLedgerStateD ldgrSt
  
-- | no methods here, 'UpdateLedger' is a class to roll-up classes:
instance UpdateLedger BlockD where {}  

instance LedgerSupportsProtocol BlockD where
  protocolLedgerView _lcfg  _tl = TickedTrivial 

  ledgerViewForecastAt _lccf = trivialForecast
    -- This is all that's needed, as 'LedgerView PrtclD' is '()'.
    -- TODO: For Prtcl D: want this to be more.
    
instance LedgerSupportsMempool BlockD where
  
  txInvariant _tx = True   -- same as default method

  applyTx _lc _wti _slot tx (TickedLedgerStateD ldgrSt) =
    return ( TickedLedgerStateD 
               ldgrSt{lsbc_count= applyTxD tx (lsbc_count ldgrSt)}
           , ValidatedTxD tx -- no evidence being provided now.
           )
    
    where
      
    -- the essence of Txs affecting ledger state:
    applyTxD (TxD Inc) i = i+1  
    applyTxD (TxD Dec) i = i-1

  reapplyTx lc slot vtx tls =
    fst <$> applyTx lc
                    (error "wti" :: WhetherToIntervene)
                    slot
                    (txForgetValidated vtx)
                    tls
    -- in general, this would *not* be an efficient way to implement reapplyTx
    
  txsMaxBytes _     = 20 -- just a random magic number for now
  txInBlockSize _tx = 2  -- post serialization size of 'tx' 
                         -- For BlockD, our Txs are all the same size
                         -- '2' probably not right
                     
  txForgetValidated (ValidatedTxD tx) = tx
    -- remove evidence of validation (currently non-existent)

---- Let's just ignore these for now -----------------------------------------

instance HasAnnTip               BlockD where {}
instance ValidateEnvelope        BlockD where {}
instance BasicEnvelopeValidation BlockD where {}


---- Data --------------------------------------------------------------------

blockD :: BlockD
blockD = BlockD { bc_header= HdrBlockD stub stub stub stub -- TODO
                , bc_body  = [TxD Inc, TxD Inc]
                }



---- header miscellanea ------------------------------------------------------
-- For Doc: just skim over.

-- | the minimum header:
data instance Header BlockD =
  HdrBlockD
    { hbc_SlotNo  :: SlotNo
    , hbc_BlockNo :: BlockNo
    , hbc_Hash    :: HeaderHash BlockD
    , hbc_prev    :: ChainHash BlockD
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockD" (Header BlockD)

instance GetHeader BlockD where
  getHeader          = bc_header
  blockMatchesHeader = \_ _ -> True -- TODO: Prtcl C/D be able to fail
  headerIsEBB        = const Nothing

instance GetPrevHash BlockD where
  headerPrevHash = hbc_prev

instance HasHeader (Header BlockD) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = hbc_SlotNo hdr
                          , headerFieldBlockNo= hbc_BlockNo hdr
                          , headerFieldHash   = hbc_Hash hdr
                          }

instance HasHeader BlockD where
  getHeaderFields = castHeaderFields       -- Q. worth some commentary?
                  . getHeaderFields
                  . bc_header
                    
type instance HeaderHash BlockD = Hash

instance StandardHash BlockD


---- Configuration boilerplate: ----------------------------------------------

data instance BlockConfig BlockD = BCfgBlockD
  deriving (Generic, NoThunks)
data instance CodecConfig BlockD = CCfgBlockD
  deriving (Generic, NoThunks)
data instance StorageConfig BlockD = SCfgBlockD
  deriving (Generic, NoThunks)



