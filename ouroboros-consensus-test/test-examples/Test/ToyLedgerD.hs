{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

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

-- | Evidence we /can/ lead slots (in general)
data PrtclD_CanBeLeader = PrtclD_CanBeLeader 

-- | Evidence we /do/ lead a particular slot.
data PrtclD_IsLeader    = PrtclD_IsLeader    
                          -- TODO: include NodeId here?
                          --  - currently never reading this!? Nick?
                          
data instance ConsensusConfig PrtclD =
  PrtclD_Config
    { ccpd_securityParam :: SecurityParam  -- ^ i.e., 'k'
    , ccpd_nodeId        :: Word64

      -- ^ A simplistic method to identify nodes, where we expect this value
      -- would be extracted from a config file on the node.
      -- 
      -- invariant: this value is unique for every node
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
       -- FUTURE:
       -- NF: How should ties be resolved?                            
       -- Discussion notes:
       --  - if two chains w/ same same blockno - which?
       --  - how do you handle ties: presently won by first block node sees,
       --    so it couldn't switch to a different chain (when BlockNo's equal).
       --  - What if we sort by blockno & node-ids?
       --  - What is a sound way?
                            
  -- | View on the ledger required by the protocol
  type LedgerView    PrtclD = LedgerViewD
  
  -- | View on a block header required for header validation
  type ValidateView  PrtclD = NodeId  -- need this for the leader check
                                      -- currently not doing other checks
                              
  type ValidationErr PrtclD = String

  -- | Am I the leader this slot?
  checkIsLeader cfg PrtclD_CanBeLeader slot tcds =
    if isLeader (ccpd_nodeId cfg) slot tcds then 
      Just PrtclD_IsLeader
    else
      Nothing
    
  protocolSecurityParam = ccpd_securityParam

  tickChainDepState _cfg tlv _slot _cds =
    tickChainDepState' tlv

  -- | apply the header (hdrView), do a header check.
  --   - here we check the block's claim to lead the slot.
  updateChainDepState _cfg hdrVw slot tcds =
    if isLeader hdrVw slot tcds then
      return ChainDepStateD
    else
      throwError $ "leader check failed: " ++ show (hdrVw,slot)
      
  reupdateChainDepState _ _ _ _ = ChainDepStateD

-- any node with same genesis block.
--  - parameter that affects leader schedule.
--  - ?
-- NF:
--  - parts of the config must be derived from the genesis block.
--  - ?

pd_config :: ConsensusConfig PrtclD
pd_config = PrtclD_Config
              { ccpd_securityParam= SecurityParam{maxRollbacks= 1}
              , ccpd_nodeId       = 0
              }

              
---- Leadership --------------------------------------------------------------

-- | A simplistic notion of identity that allows for round-robin leader selection.
--  NodeId's of 0..19 are engaged in an alternating round-robin (see 'isLeader').
type NodeId = Word64 
  
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

  selectView   _bcfg hdr = blockNo hdr
                            -- i.e., the default method.
                            -- i.e., we only need the blockNo to do chain selection
                            -- see 'preferCandidate'
  

-- The transaction type for BlockD (same as for BlockC)

data Tx = Inc | Dec
  deriving (Show, Eq, Generic, Serialise, NoThunks, Hashable)

data instance GenTx BlockD = TxD Tx
  deriving (Show, Eq, Generic, Serialise, NoThunks, Hashable)


-- | And what it means for the transaction to be Validated (trivial for now)
--   Note that Validated must include the transaction as well as the evidence

data instance Validated (GenTx BlockD) = ValidatedTxD (GenTx BlockD)
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks


---- A simplistic concept of Epochs ------------------------------------------

slotsInEpoch :: Word64
slotsInEpoch = 50
  -- TODO: more interesting to put this into LedgerCfg?
  -- - yes, but Genesis block: has initial values for _, then on-chain changes
  -- - just block specific (not in Ledger).
  -- consensus config for a block must have initial values.
  --  - 'k' = K, not expected
  --  - k into consensusConfig (or ledgerConfig!)
  --  - if changed by voting/_ then in the Ledger
  --  - d parameter for Shelley

-- | epochOf - Note that we preserve the 'WithOrigin in the result', we don't
-- want to associate an arbitrary EpochNo with 'Origin', as we try hard to avoid
-- "ignoring" Origin.
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
    { lsbd_tip   :: Point BlockD  -- Point of the last applied block.
                                  --   (Point is header hash and slot num)
    , lsbd_count    :: Word64     -- results of the up/down Txs
    , lsbd_snapshot :: Word64     -- snapshot of lsbd_count, made at epoch
                                  --   boundaries
                                  -- NF: Can we move this into ChainDepState?
                                  -- Nick: I do not see a way.
    }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

newtype instance Ticked (LedgerState BlockD) =
  TickedLedgerStateD {
    unTickedLedgerStateD :: LedgerState BlockD
  }
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- | We want to define a 'LedgerView p' and a 'Ticked(LedgerView p)'.

-- | Our LedgerView will be 'lsbd_snapshot' (see above), so the type we want is

newtype LedgerViewD = LVD Word64 
  deriving (Show, Eq, Generic, Serialise, NoThunks)

-- | Ticking LedgerViewD requires no less, no more than LedgerViewD:

newtype instance Ticked LedgerViewD = TickedLedgerViewD LedgerViewD
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
  protocolLedgerView _ldgrCfg (TickedLedgerStateD ldgrSt) =
    TickedLedgerViewD (LVD $ lsbd_snapshot ldgrSt)

  -- | Borrowing somewhat from Ouroboros/Consensus/Byron/Ledger/Ledger.hs
  ledgerViewForecastAt _lccf ldgrSt =
    Forecast { forecastAt= at
             , forecastFor= \for->
                 if NotOrigin for < at then
                    error "precondition violated: 'NotOrigin for >= at'"
                 else if for >= maxFor then
                   throwError
                     OutsideForecastRange
                        { outsideForecastAt    = at
                        , outsideForecastMaxFor= maxFor
                        , outsideForecastFor   = for
                        }
                 else
                   return $ TickedLedgerViewD $ LVD $ lsbd_snapshot ldgrSt
             }

    where
    -- | the current slot that the ledger reflects
    at :: WithOrigin SlotNo
    at = pointSlot $ lsbd_tip $ ldgrSt  

    -- | 'maxFor' will be the start of the next epoch, i.e., the slot of the
    -- next snapshot.  This is when the LedgerView becomes unknown.
    maxFor :: SlotNo
    maxFor = nextEpochStartSlot at
    
    {-
    FUTURE:
    The above 'Forecast {}' value is somewhat unsatisfying:
     - At the last slot in an epoch, we cannot forecast (beyond current slot)
       - Will the protocol get stuck?
       - Even if sound, is this practical?
       - FUTURE WORK: we would like formulate some properties of this method
         (with other methods possibly) by which we could be assured of soundness.
    
     - A more usable ledger would ensure maxFor is _always_ >> 0.
    
    Alternatives for improving the forecast:
     A. Have two snapshots so we always have one epoch of lead time!
       - then 'maxfor' significantly greater than 'at' (the older snapshot has
         the leader schedule).
     B. When 70% through the epoch, we do the snapshot.
    -}


---- Examples & Testing: ledgerViewForecastAt --------------------------------

ldgrAtSlot :: SlotNo -> LedgerState BlockD
ldgrAtSlot slot =
  LedgerC{ lsbd_tip     = Point (NotOrigin (Block slot (Hash 0)))
         , lsbd_count   = 5
         , lsbd_snapshot= 0
         }

fc :: SlotNo -> Forecast LedgerViewD
fc ldgrTip = ledgerViewForecastAt () (ldgrAtSlot ldgrTip)

testfc :: Word64 -> Word64 -> Except OutsideForecastRange (Ticked LedgerViewD)
testfc at' for = forecastFor (fc (SlotNo at')) (SlotNo for)

{- | 
>>> mapM_ (\i-> print $ testfc 49 i) [49,50,51]

ExceptT (Identity (Right (TickedLedgerViewD (LVD 0))))
ExceptT (Identity (Left (OutsideForecastRange {outsideForecastAt = At (SlotNo 49), outsideForecastMaxFor = SlotNo 50, outsideForecastFor = SlotNo 50})))
ExceptT (Identity (Left (OutsideForecastRange {outsideForecastAt = At (SlotNo 49), outsideForecastMaxFor = SlotNo 50, outsideForecastFor = SlotNo 51})))

I.e., at slot 49 we can forecast at slot 49 but no further.
-}

---- Transactions and Mempools -----------------------------------------------

instance LedgerSupportsMempool BlockD where
  
  txInvariant _tx = True   -- equivalent to the default method

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
                     
  txForgetValidated (ValidatedTxD tx) = tx
    -- remove evidence of validation (currently non-existent)


---- Let's just ignore these for now -----------------------------------------

instance HasAnnTip               BlockD where {}
instance ValidateEnvelope        BlockD where {}
instance BasicEnvelopeValidation BlockD where {}


---- Examples & Testing: blocks ----------------------------------------------

blockD :: BlockD
blockD =
  BlockD { bd_header= HdrBlockD{ hbd_SlotNo = SlotNo 10
                               , hbd_BlockNo= BlockNo 8
                               , hbd_Hash   = hash' body
                               , hbd_prev   = BlockHash (Hash 1)
                               , hbd_nodeId = 5
                               }
         , bd_body  = body
         }
  where
  body = [TxD Inc, TxD Inc]

testBlockD :: Bool
testBlockD = blockMatchesHeader (bd_header blockD) blockD

---- header miscellanea ------------------------------------------------------
-- For Doc: just skim over.

-- | the minimum header:
data instance Header BlockD =
  HdrBlockD
    { hbd_SlotNo  :: SlotNo
    , hbd_BlockNo :: BlockNo
    , hbd_Hash    :: HeaderHash BlockD
    , hbd_prev    :: ChainHash BlockD
    
    , hbd_nodeId  :: NodeId  -- this is specific to PrtclD, we need this for
                             -- the leader proof/check.
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockD" (Header BlockD)

instance GetHeader BlockD where
  getHeader          = bd_header
  
  blockMatchesHeader hdr blk =
    hbd_Hash hdr == hash' (bd_body blk)
        
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



