{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ToyLedgerD where

-- base pkgs:
import           Control.Monad
import           Control.Monad.Except
import           Data.Word (Word64)
import           GHC.Generics (Generic)

-- pkg nothunks:
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

-- pkg hashable:
import           Data.Hashable

-- pkg serialise:
import           Codec.Serialise

-- pkg ouroboros-network:
import           Ouroboros.Network.Point  hiding (at)
import           Ouroboros.Network.Block

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

-- | A simplistic notion of node identity that allows for round-robin leader
--  selection.  NodeId's of 0..19 are engaged in an alternating round-robin (see
--  'isLeader').  All nodes that can lead must have an identifier.
type NodeId = Word64 
  
-- | Evidence that we /can/ lead slots (in general)
--
-- NOTE: In PrtclD, the evidence is easily forged, but we will use the NodeId, in the
--  range [0..19].
data PrtclD_CanBeLeader = PrtclD_CanBeLeader NodeId
                          deriving (Eq, Show, Generic, NoThunks)

-- | Evidence that we /do/ lead a particular slot.
-- 
-- NOTE:
--   In Shelley, for instance, this would be a crypto proof we lead a slot,
--   and that proof would be in the block header.

data PrtclD_IsLeader    = PrtclD_IsLeader
                          
data instance ConsensusConfig PrtclD =
  PrtclD_Config
    { ccpd_securityParam :: SecurityParam  -- ^ i.e., 'k'
    , ccpd_nodeId        :: Maybe PrtclD_CanBeLeader

      -- ^ Leader nodes must have a 'ccpd_nodeId' equal to 'Just (PrtclD_CanBeLeader nodeid)',
      -- We expect this value would be extracted from a config file.
      -- 
      -- Invariant: nodeid's are unique.
    }
  deriving (Eq, Show)
  deriving NoThunks via OnlyCheckWhnfNamed "PrtclD_Config"
                        (ConsensusConfig PrtclD)

instance ConsensusProtocol PrtclD where
  
  type ChainDepState PrtclD = ChainDepStateD
  type IsLeader      PrtclD = PrtclD_IsLeader
  type CanBeLeader   PrtclD = PrtclD_CanBeLeader
  
  -- | View on a block header required for chain selection.  Here, BlockNo is
  --   sufficient. (BlockNo is also the default type for this type family.)
  type SelectView    PrtclD = BlockNo
       -- FUTURE:
       -- Discussion of the ramifications of the simple BlockNo:
       --  - If two chains have same same blockno - which?
       --  - Here, a tie is won by the first block that a node sees,
       --    so it couldn't switch to a different chain (when BlockNo's equal).
       --  - What if we sort by blockno & node-ids?
       --  - What is a sound way?
                            
  -- | View on the ledger required by the protocol
  type LedgerView    PrtclD = LedgerViewD
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclD = NodeId  -- need this for the leader check
                                      -- currently not doing other checks
                              
  type ValidationErr PrtclD = String

  -- | checkIsLeader - Am I the leader this slot?
  -- 
  -- NOTE: For Protocol D
  --   - there is no real evidence of leadership in this protocol
  --   - what might be akin to a private key (for generating evidence) is
  --     extracted from the configuration using 'cpd_nodeId cfg'
  --   thus, for this simple example, we ignore the '_cbl :: PrtclD_CanBeLeader' argument.
  --
  -- NOTE: In a more realistic Protocol
  --  - We would expect:
  --    - 'checkIsLeader' to be creating IsLeader evidence from CanBeLeader evidence.
  --    - the IsLeader evidence to end up in the block header (as this method will be invoked
  --      as part of block forging).
  --  - From the point of view of consumers of headers,
  --    - The IsLeader evidence would be a part of the ValidateView projection of the header.
  --    - (As an aside: in the real system, the whole header is the ValidateView.)
  
  checkIsLeader cfg _cbl slot tcds =
    case ccpd_nodeId cfg of
      Just (PrtclD_CanBeLeader nodeId)
        | isLeader nodeId slot tcds -> Just PrtclD_IsLeader  -- not providing any proof
      _                             -> Nothing
      
  protocolSecurityParam = ccpd_securityParam

  tickChainDepState _cfg tlv _slot _cds =
    tickChainDepState' tlv

  -- | apply the header (hdrView) and do a header check.
  -- 
  -- Here we check the block's claim to lead the slot (though in Protocol D,
  -- this doesn't give us too much confidence, as there is nothing that
  -- precludes a node from masquerading as any other node).
  
  updateChainDepState _cfg hdrVw slot tcds =
    if isLeader hdrVw slot tcds then
      return ChainDepStateD
    else
      throwError $ "leader check failed: " ++ show (hdrVw,slot)
      
  reupdateChainDepState _ _ _ _ = ChainDepStateD


pd_config :: ConsensusConfig PrtclD
pd_config = PrtclD_Config
              { ccpd_securityParam= SecurityParam{maxRollbacks= 1}
              , ccpd_nodeId       = Just (PrtclD_CanBeLeader 0)
              }

              
---- Leadership --------------------------------------------------------------

-- | 'ChainDepState PrtclD' is effectively unit for the moment.
--   FUTURE: Extend type to make more realistic.
data ChainDepStateD = ChainDepStateD
                      deriving (Eq,Show,Generic,NoThunks)

-- | Our Ticked ChainDepStateD must contain the LedgerViewD, this allows us to
--   base the leadership schedule on the LedgerState (at the last epoch boundary).
data instance Ticked ChainDepStateD = TickedChainDepStateD LedgerViewD
                                      deriving (Eq,Show,Generic,NoThunks)


-- | A somewhat degenerate tickChainDepState function, but here we simply want to
--   extract the relevant LedgerView data into our 'Ticked ChainDepStateD':
tickChainDepState' :: Ticked LedgerViewD -> Ticked ChainDepStateD
tickChainDepState' (TickedLedgerViewD lv) = TickedChainDepStateD lv
    

-- | A somewhat fanciful leadership schedule, each epoch chooses a particular
--   set of 10 nodes to do a round-robin schedule. This set we choose is based
--   on whether the ledger state (the LedgerView actually), a single counter, is
--   odd or even.
isLeader :: NodeId -> SlotNo -> Ticked ChainDepStateD -> Bool
isLeader nodeId (SlotNo slot) (TickedChainDepStateD (LVD cntr)) =
  case cntr `mod` 2 of
    0 -> slot `mod` 10      == nodeId  -- nodes [0..9]   do round-robin (if even cntr)
    1 -> (slot `mod` 10)+10 == nodeId  -- nodes [10..19] do round-robin (if odd cntr)
    _ -> error "panic: the impossible ..."

         
---- Block D (for Protocol D) ------------------------------------------------

-- | Define BlockD
data BlockD = BlockD { bd_header :: Header BlockD
                     , bd_body   :: [GenTx BlockD]
                     }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockD" BlockD


-- associate PrtclD to BlockD:

type instance BlockProtocol BlockD = PrtclD

instance BlockSupportsProtocol BlockD where
  
  -- | Gives projection of the header needed to do header validation.
  --   In PrtclD, just give what's needed for leader check.

  validateView _bcfg hdr = hbd_nodeId hdr

  -- | 'selectView' here is just the default method.
  -- I.e., we only need the blockNo to do chain selection,
  -- this is used by 'preferCandidate'.
  
  selectView   _bcfg hdr = blockNo hdr
  

-- | The transaction type for BlockD (same as for BlockC)
data Tx = Inc | Dec
  deriving (Show, Eq, Generic, Serialise, NoThunks, Hashable)


data instance GenTx BlockD = TxD Tx
  deriving (Show, Eq, Generic, Serialise, NoThunks, Hashable)


-- | Defining what it means for the transaction to be Validated (trivial for now):
--   Note that Validated must include the transaction as well as the evidence.
data instance Validated (GenTx BlockD) = ValidatedTxD (GenTx BlockD)
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks


---- A simple concept of Epochs ----------------------------------------------

-- | slotsInEpoch -
-- 
-- For Protocol D, we make this parameter simple, it is defined at the top level
-- and is thus fixed for all eternity:
-- 
--   It's not in the 'ConsensusConfig', it cannot affect the 'LedgerState', it
--   is not determined by anything in the genesis block [not sure I know what
--   I'm talking about here].
--
-- FUTURE:
-- The Protocol could be extended to be more complex in multiple ways:
--   1. If 'slotsInEpoch' was, rather, part of the ConsensusConfig,
--     - Each node must be configured the same, otherwise a node would be part of a different blockchain.
--     - Possibly this config value would be determined from the Genesis block?
--   2. Furthermore, if we wanted 'slotsInEpoch' to be mutable:
--     - it would need to be in the LedgerState and more transactions (such as voting) would be
--       required to allow for this.
-- 
slotsInEpoch :: Word64
slotsInEpoch = 50


-- | epochOf - Note that we preserve the 'WithOrigin' in the result, we don't
-- want to associate an arbitrary EpochNo with 'Origin', as we try hard to avoid
-- ignoring Origin in the Ouroboros codebase.
epochOf :: WithOrigin SlotNo -> WithOrigin EpochNo
epochOf Origin        = Origin
epochOf (NotOrigin s) = NotOrigin $ EpochNo $ unSlotNo s `div` slotsInEpoch
                        
nextEpochStartSlot :: WithOrigin SlotNo -> SlotNo
nextEpochStartSlot wo =
  SlotNo $ case wo of
             Origin         -> slotsInEpoch
             NotOrigin slot -> slotsInEpoch + slot' - (slot' `mod` slotsInEpoch)
                               where
                               slot' = unSlotNo slot


---- The Ledger State for Block D: Type family instances ---------------------

data instance LedgerState BlockD =
  LedgerC
    { lsbd_tip    :: Point BlockD  -- ^ Point of the last applied block.
                                   --   (Point is header hash and slot no.)
    , lsbd_count     :: Word64     -- ^ results of the up/down Txs
    , lsbd_snapshot1 :: Word64     -- ^ snapshot of lsbd_count at
                                   --   end of previous epoch (1 epoch ago)
    , lsbd_snapshot2 :: Word64     -- ^ snapshot of lsbd_count at end
                                   --   of epoch (2 epochs ago)
                                   --   This will be the LedgerView that
                                   --   influences the leader schedule.
    }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

newtype instance Ticked (LedgerState BlockD) =
  TickedLedgerStateD {
    unTickedLedgerStateD :: LedgerState BlockD
  }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- | We need to define a 'LedgerView p' and a 'Ticked(LedgerView p)'.

-- | Our LedgerView will be 'lsbd_snapshot2' (see above), so the type we
-- want is the following:

newtype LedgerViewD = LVD Word64 
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- | Ticking LedgerViewD requires no less, no more than LedgerViewD:

newtype instance Ticked LedgerViewD = TickedLedgerViewD LedgerViewD
  deriving (Show, Eq, Generic, Serialise, NoThunks)


-- We need no LedgerCfg data:
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
                   , lrResult= ldgrSt{lsbd_tip= blockPoint b}
                   }
    where
    slot = blockSlot (getHeader b)

  reapplyBlockLedgerResult ldgrCfg b tickedLdgrSt =
    case runExcept $ applyBlockLedgerResult ldgrCfg b tickedLdgrSt of
      Left s  -> error $ "impossible! reapplyBlockLedgerResult: " ++ s
      Right x -> x
      
    -- ASIDE:
    --   We're not taking advantage of the opportunity of being more
    --   efficient than 'applyBlockLedgerResult' by skipping checks.  But this
    --   is not needed for Protocol D.


-- | tickLedgerStateD - helper function to tick the LedgerState. Here, in
--   Protocol/Ledger D, we 'snapshot' the Ledger state (i.e., 'lsbd_count') at
--   epoch boundaries (to be used for leader selection).

tickLedgerStateD ::
  SlotNo -> LedgerState BlockD -> Ticked (LedgerState BlockD)
tickLedgerStateD newSlot ldgrSt =
  TickedLedgerStateD $
    if isNewEpoch then
      ldgrSt{ lsbd_snapshot2= lsbd_snapshot1 ldgrSt
               -- save previous epoch snapshot (assumes we cannot
               -- go an epoch without ticking)
            , lsbd_snapshot1= lsbd_count ldgrSt
               -- snapshot the count (at end of previous epoch)
            }
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
  protocolLedgerView _ldgrCfg (TickedLedgerStateD ldgrSt) =
    TickedLedgerViewD (LVD $ lsbd_snapshot2 ldgrSt)
       -- note that we use a snapshot from 2 epochs ago.

  -- | Borrowing somewhat from Ouroboros/Consensus/Byron/Ledger/Ledger.hs
  ledgerViewForecastAt _lccf ldgrSt =
    Forecast { forecastAt= at
             , forecastFor= \for->
                 if NotOrigin for < at then
                   error "this precondition violated: 'NotOrigin for < at'"
                 else if for >= maxFor then
                   throwError
                     OutsideForecastRange
                        { outsideForecastAt    = at
                        , outsideForecastMaxFor= maxFor
                        , outsideForecastFor   = for
                        }
                 else
                   return
                     $ TickedLedgerViewD $ LVD
                     $ if for < nextEpochStartSlot at then
                         lsbd_snapshot2 ldgrSt
                         -- same as 'protocolLedgerView' for the rest of the
                         -- current epoch (i.e., using snapshot from 2 epochs
                         -- ago):
                       else
                         lsbd_snapshot1 ldgrSt
                         -- we can forecast into the next epoch because
                         -- we have the snapshot for the previous epoch
             }

    where
    -- | the current slot that the ledger reflects
    at :: WithOrigin SlotNo
    at = pointSlot $ lsbd_tip $ ldgrSt  

    -- | 'maxFor' is the "exclusive upper bound on the range of the forecast"
    -- (the name "max" does seem wrong, but we are following suit with the names
    -- and terminology in the 'Ouroboros.Consensus.Forecast' module)
    --
    -- In our case we can forecast for the current epoch and the following
    -- epoch.  The forecast becomes unknown at the start of the epoch
    -- after the following epoch:
    maxFor :: SlotNo
    maxFor = nextEpochStartSlot at + SlotNo slotsInEpoch
    

---- Examples & Testing: ledgerViewForecastAt --------------------------------

ldgrAtSlot :: SlotNo -> LedgerState BlockD
ldgrAtSlot slot =
  LedgerC{ lsbd_tip      = Point (NotOrigin (Block slot (Hash 0)))
         , lsbd_count    = 5
         , lsbd_snapshot1= 0
         , lsbd_snapshot2= 0
         }

makeForecast :: SlotNo -> Forecast LedgerViewD
makeForecast tipslot = ledgerViewForecastAt () (ldgrAtSlot tipslot)

testForecast :: Word64 -> Word64 -> Except OutsideForecastRange (Ticked LedgerViewD)
testForecast at' for = forecastFor (makeForecast (SlotNo at')) (SlotNo for)

{- | 

In this test, slot 50 is next epoch, slot 100 is epoch following that.
From slot 49 (the last slot in epoch 0), we can forecast through slot 99
(the last slot in epoch 1):

>>> mapM_ (\i-> print $ (i, epochOf(At(SlotNo i)), testForecast 49 i)) [49,50,51,99,100]

(49,At (EpochNo 0),ExceptT (Identity (Right (TickedLedgerViewD (LVD 0)))))
(50,At (EpochNo 1),ExceptT (Identity (Right (TickedLedgerViewD (LVD 0)))))
(51,At (EpochNo 1),ExceptT (Identity (Right (TickedLedgerViewD (LVD 0)))))
(99,At (EpochNo 1),ExceptT (Identity (Right (TickedLedgerViewD (LVD 0)))))
(100,At (EpochNo 2),ExceptT (Identity (Left (OutsideForecastRange {outsideForecastAt = At (SlotNo 49), outsideForecastMaxFor = SlotNo 100, outsideForecastFor = SlotNo 100}))))
-}


---- Transactions and Mempools -----------------------------------------------

instance LedgerSupportsMempool BlockD where

  -- | equivalent to the default method:
  txInvariant _tx = True   

  applyTx _lc _wti _slot tx (TickedLedgerStateD ldgrSt) =
    return ( TickedLedgerStateD 
               ldgrSt{lsbd_count= applyTxD tx (lsbd_count ldgrSt)}
           , ValidatedTxD tx -- no evidence currently being provided.
           )
    -- FUTURE: expand with Tx's that can fail.
    
    where
      
    -- | the essence of how Txs affect the ledger state:
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
                     
  -- | remove evidence of validation (currently no evidence)
  txForgetValidated (ValidatedTxD tx) = tx


---- Let's just ignore these for now -----------------------------------------

instance HasAnnTip               BlockD where {}
instance ValidateEnvelope        BlockD where {}
instance BasicEnvelopeValidation BlockD where {}


---- Examples & Testing: blocks ----------------------------------------------

blockD :: BlockD
blockD = addBlockHash $
           BlockD 
             HdrBlockD{ hbd_SlotNo = SlotNo 10
                      , hbd_BlockNo= BlockNo 8
                      , hbd_Hash   = error "panic: shouldn't be read"
                      , hbd_prev   = BlockHash (Hash 1)  -- header hash is hash of header & block
                      , hbd_nodeId = 5
                      }
             [TxD Inc, TxD Inc]  

testBlockD :: Bool
testBlockD = blockMatchesHeader (bd_header blockD) blockD

---- Hashing BlockD ----------------------------------------------------------

addBlockHash :: BlockD -> BlockD
addBlockHash b = 
  BlockD { bd_header= (bd_header b){hbd_Hash = computeBlockHash b}
         , bd_body  = bd_body b
         }

computeBlockHash :: BlockD -> Hash
computeBlockHash (BlockD hdr body) = hash' (hdr{hbd_Hash=Hash 0}, body)

-- and the Hashable instances needed for this:

deriving instance Hashable (Header BlockD)
deriving instance Hashable (ChainHash BlockD)
deriving instance Hashable SlotNo
deriving instance Hashable BlockNo


---- header miscellanea ------------------------------------------------------

-- | the minimum header:
data instance Header BlockD =
  HdrBlockD
    -- generic fields we need for all protocols:
    { hbd_SlotNo  :: SlotNo
    , hbd_BlockNo :: BlockNo
    , hbd_Hash    :: HeaderHash BlockD -- hash of whole block (excepting this field)
    , hbd_prev    :: ChainHash BlockD

    -- this is specific to PrtclD, we need this for the leader proof/check:
    , hbd_nodeId  :: NodeId 
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockD" (Header BlockD)

instance GetHeader BlockD where
  getHeader          = bd_header
  
  blockMatchesHeader hdr blk =
    hbd_Hash hdr == computeBlockHash blk

  -- | Protocol D has no EBBs, fortunately.
  headerIsEBB      _ = Nothing

instance GetPrevHash BlockD where
  headerPrevHash = hbd_prev

instance HasHeader (Header BlockD) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = hbd_SlotNo hdr
                          , headerFieldBlockNo= hbd_BlockNo hdr
                          , headerFieldHash   = hbd_Hash hdr
                          }

instance HasHeader BlockD where
  getHeaderFields = castHeaderFields
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
