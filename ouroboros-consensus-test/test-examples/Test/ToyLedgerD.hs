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
import           Control.Monad.Except
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
import           Ouroboros.Consensus.Forecast

-- local modules:
import           Test.Utilities


---- Define Protocol D : 'PrtclD' --------------------------------------------

data PrtclD         

data PrtclD_CanBeLeader = PrtclD_CanBeLeader -- Evidence we /can/ be a leader
data PrtclD_IsLeader    = PrtclD_IsLeader    -- Evidence we /are/ leader

data instance ConsensusConfig PrtclD =
  PrtclD_Config
    { ccpd_securityParam :: SecurityParam  -- i.e., 'k'
    , ccpd_nodeId        :: Word64         -- simplistic method to identify nodes
                                             -- invariant: this unique for every node
    }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclD_Config"
                        (ConsensusConfig PrtclD)

instance ConsensusProtocol PrtclD where
  
  type ChainDepState PrtclD = ChainDepStateD
  type IsLeader      PrtclD = PrtclD_IsLeader
  type CanBeLeader   PrtclD = PrtclD_CanBeLeader
  
  -- | View on a block header required for chain selection.
  --   Here, BlockNo is sufficient (also the default):
  type SelectView    PrtclD = BlockNo

  -- | View on the ledger required by the protocol
  type LedgerView    PrtclD = LedgerViewD
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclD = ()               -- TODO: make non-trivial.
  type ValidationErr PrtclD = Void

  checkIsLeader cfg PrtclD_CanBeLeader slot tcds =
    if isLeader (ccpd_nodeId cfg) slot tcds then
      Just PrtclD_IsLeader
    else
      Nothing
    
  protocolSecurityParam = ccpd_securityParam

  tickChainDepState _cfg tlv _slot _cds =
    tickChainDepState' tlv

  updateChainDepState   _ _ _ _ = return ChainDepStateD
  
  reupdateChainDepState _ _ _ _ = ChainDepStateD


pd_config :: ConsensusConfig PrtclD
pd_config = PrtclD_Config
              { ccpd_securityParam= SecurityParam{maxRollbacks= 1}
              , ccpd_nodeId       = 0
              }

              
---- Leadership --------------------------------------------------------------

-- | 'ChainDepState PrtclD' is unit for the moment.
--   TODO: extend to make more realistic.
data ChainDepStateD = ChainDepStateD
     deriving (Eq,Show,Generic,NoThunks)

-- | Our Ticked ChainDepStateD must contain the LedgerViewD, this allows us to
--   base the leadership schedule on the LedgerState (at the last epoch boundary).
data instance Ticked ChainDepStateD = TickedChainDepStateD LedgerViewD

-- | A rather degenerate tickChainDepState function, but here we simply want to
--   extract the relevant LedgerView data into our Ticked ChainDepState:
tickChainDepState' :: Ticked LedgerViewD -> Ticked ChainDepStateD
tickChainDepState' (TickedLedgerViewD lv) = TickedChainDepStateD lv
    

-- | A somewhat fanciful leadership schedule, each epoch chooses a different 10
--   nodes to do a round-robin schedule:
isLeader :: Word64 -> SlotNo -> Ticked ChainDepStateD -> Bool
isLeader nodeId (SlotNo slot) (TickedChainDepStateD x) =
  case x `mod` 2 of
    0 -> slot `mod` 10      == nodeId  -- nodes [0..9]   do a round-robin
    1 -> (slot `mod` 10)+10 == nodeId  -- nodes [10..19] do a round-robin

         
---- Block D (for Protocol D) ------------------------------------------------

-- | Define BlockD
data BlockD = BlockD { bd_header :: Header BlockD
                     , bd_body   :: [GenTx BlockD]
                     }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockD" BlockD


-- associate BlockD to the protocol PrtclD:

type instance BlockProtocol BlockD = PrtclD

instance BlockSupportsProtocol BlockD where
  validateView _bcfg _hdr = ()
  selectView   _bcfg hdr  = blockNo hdr
                            -- i.e., the default method.
                            -- i.e., we only need the blockNo to do chain selection
                            -- see ...Protocol.Abstract.preferCandidate
  

-- The transaction type for BlockD (same as for BlockC)

data Tx = Inc | Dec
  deriving (Show, Eq, Generic, Serialise, NoThunks)

data instance GenTx BlockD = TxD Tx
  deriving (Show, Eq, Generic, Serialise, NoThunks)


-- | And what it means for the transaction to be Validated (trivial for now)
--   Note that Validated must include the transaction as well as the evidence

data instance Validated (GenTx BlockD) = ValidatedTxD (GenTx BlockD)
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks


---- A simplistic concept of Epochs ------------------------------------------

slotsInEpoch :: Word64
slotsInEpoch = 50
  -- TODO: more interesting to put this into LedgerCfg?

epochOf :: WithOrigin SlotNo -> EpochNo
epochOf Origin        = EpochNo 0  -- Appears safe to put Origin into epoch 0
epochOf (NotOrigin s) = EpochNo $ unSlotNo s `mod` slotsInEpoch
                        
nextEpochStartSlot :: WithOrigin SlotNo -> SlotNo
nextEpochStartSlot wo =
  SlotNo $ case wo of
             Origin         -> slotsInEpoch
             NotOrigin slot -> (unSlotNo slot `div` slotsInEpoch) + slotsInEpoch


---- The Ledger State for Block D: Type family instances ---------------------

data instance LedgerState BlockD =
  LedgerC
    { lsbd_tip   :: Point BlockD  -- Point for the last applied block.
                                  --   (Point is header hash and slot num)
    , lsbd_count    :: Word64     -- results of the up/down Txs
    , lsbd_snapshot :: Word64     -- snapshot of lsbd_count, made at epoch
                                  --   boundaries
    }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

newtype instance Ticked (LedgerState BlockD) =
  TickedLedgerStateD {
    unTickedLedgerStateD :: LedgerState BlockD
  }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- | We want to define a 'LedgerView p' and a 'Ticked(LedgerView p)'.

-- | Our LedgerView will be 'lsbd_snapshot' (see above), so the type we want is

type LedgerViewD = Word64 

-- | Ticking LedgerViewD requires no less, no more than LedgerViewD:

data instance Ticked LedgerViewD = TickedLedgerViewD LedgerViewD
  deriving (Show, Eq, Generic, Serialise, NoThunks)


-- No LedgerCfg data:
type instance LedgerCfg (LedgerState BlockD) = ()

type LedgerErr_BlockD = String
  
type instance ApplyTxErr BlockD = LedgerErr_BlockD


---- The Ledger State for Block D: class instances ---------------------------

instance GetTip (Ticked (LedgerState BlockD)) where
  getTip = castPoint . lsbd_tip . unTickedLedgerStateD

instance GetTip (LedgerState BlockD) where
  getTip = castPoint . lsbd_tip


instance IsLedger (LedgerState BlockD) where
  type instance LedgerErr      (LedgerState BlockD) = LedgerErr_BlockD 
  type instance AuxLedgerEvent (LedgerState BlockD) = ()

  -- | This method is for updating the ledger state when it needs to change
  -- based on time (slot) changing.  For Protocol/Ledger D, we want to take a snapshot
  -- at epoch boundaries, see the definition of 'tickLedgerStateD' below.
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
        (bd_body b)
      
    return $
      LedgerResult { lrEvents= []
                   , lrResult= ldgrSt
                   }
    where
    slot = blockSlot (getHeader b)

    -- NOTE: the tick function does not update tip!
    -- FIXME: update lsbd_tip!
    
  reapplyBlockLedgerResult _lc b _tl =
    LedgerResult { lrEvents= []
                 , lrResult= stub b
                 }
    -- TODO(low-priority): fill in; though this would be primarily boilerplate.
    -- 
    -- ASIDE:
    --   We're not planning to implement this "realistically", i.e., to make
    --   this particularly faster than 'applyBlockLedgerResult'


-- | tickLedgerStateD - helper function to tick the LedgerState. Here, in
--   Protocol/Ledger D, we 'snapshot' the Ledger state (i.e., 'lsbd_count') at
--   epoch boundaries (to use by the leader selection).

tickLedgerStateD ::
  SlotNo -> LedgerState BlockD -> Ticked (LedgerState BlockD)
tickLedgerStateD newSlot ldgrSt =
  TickedLedgerStateD $
    if isNewEpoch then
      ldgrSt{lsbd_snapshot= lsbd_count ldgrSt}  -- snapshot the count
    else
      ldgrSt 
      
  where
  isNewEpoch =
    case compare
           (epochOf (pointSlot $ lsbd_tip ldgrSt)) -- epoch of last block added
           (epochOf (NotOrigin newSlot))           -- epoch of newSlot
    of
      LT -> True
      EQ -> False
      GT -> error "cannot tick slots backwards"
    
    
-- | no methods here, 'UpdateLedger' is a class to roll-up classes:
instance UpdateLedger BlockD where {}  


instance LedgerSupportsProtocol BlockD where
  protocolLedgerView _lcfg (TickedLedgerStateD ldgrSt) =
    TickedLedgerViewD (lsbd_snapshot ldgrSt)

  -- | Borrowing somewhat from Ouroboros/Consensus/Byron/Ledger/Ledger.hs
  ledgerViewForecastAt _lccf ldgrSt =
    Forecast { forecastAt= at
             , forecastFor= \for->
                 if NotOrigin for < at then
                    error "panic: precondition violated"
                 else if for >= maxFor then
                   throwError $                      
                     OutsideForecastRange
                        { outsideForecastAt    = at
                        , outsideForecastMaxFor= maxFor
                        , outsideForecastFor   = for
                        }
                 else
                   return $ TickedLedgerViewD $ lsbd_snapshot ldgrSt
             }
    where
    at :: WithOrigin SlotNo
    at = pointSlot $ lsbd_tip $ ldgrSt  -- the current slot that the ledger reflects

    maxFor :: SlotNo
    maxFor = nextEpochStartSlot at  -- next epoch is time of next snapshot
                                    -- when our LedgerView becomes unknown.

    
instance LedgerSupportsMempool BlockD where
  
  txInvariant _tx = True   -- same as default method

  applyTx _lc _wti _slot tx (TickedLedgerStateD ldgrSt) =
    return ( TickedLedgerStateD 
               ldgrSt{lsbd_count= applyTxD tx (lsbd_count ldgrSt)}
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
blockD = BlockD { bd_header= HdrBlockD stub stub stub stub -- TODO(low-priority)
                , bd_body  = [TxD Inc, TxD Inc]
                }



---- header miscellanea ------------------------------------------------------
-- For Doc: just skim over.

-- | the minimum header:
data instance Header BlockD =
  HdrBlockD
    { hbd_SlotNo  :: SlotNo
    , hbd_BlockNo :: BlockNo
    , hbd_Hash    :: HeaderHash BlockD
    , hbd_prev    :: ChainHash BlockD
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockD" (Header BlockD)

instance GetHeader BlockD where
  getHeader          = bd_header
  blockMatchesHeader = \_ _ -> True -- TODO: Prtcl C/D be able to fail
  headerIsEBB        = const Nothing

instance GetPrevHash BlockD where
  headerPrevHash = hbd_prev

instance HasHeader (Header BlockD) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = hbd_SlotNo hdr
                          , headerFieldBlockNo= hbd_BlockNo hdr
                          , headerFieldHash   = hbd_Hash hdr
                          }

instance HasHeader BlockD where
  getHeaderFields = castHeaderFields       -- Q. worth some commentary?
                  . getHeaderFields
                  . bd_header
                    
type instance HeaderHash BlockD = Hash

instance StandardHash BlockD


---- Configuration boilerplate: ----------------------------------------------

data instance BlockConfig BlockD = BCfgBlockD
  deriving (Generic, NoThunks)
data instance CodecConfig BlockD = CCfgBlockD
  deriving (Generic, NoThunks)
data instance StorageConfig BlockD = SCfgBlockD
  deriving (Generic, NoThunks)



