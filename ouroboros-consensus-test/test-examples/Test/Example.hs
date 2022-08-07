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

---- Define Protocol 'SP' - Simplest Protocol --------------------------------

data SP             -- The Simplest Protocol

data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
data SP_IsLeader    = SP_IsLeader    -- Evidence that we /are/ leader

data instance ConsensusConfig SP =
  SP_Config { cfgsp_iLeadInSlots  :: Set SlotNo
            , cfgsp_securityParam :: SecurityParam
            }
  deriving (Eq, Show)
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

  protocolSecurityParam = cfgsp_securityParam

  tickChainDepState     _ _ _ _ = TickedTrivial
                                  -- works b/c ChainDepState SP = ()
                                  
  updateChainDepState   _ _ _ _ = return ()
  
  reupdateChainDepState _ _ _ _ = ()


sp_config :: ConsensusConfig SP
sp_config = SP_Config
              { cfgsp_iLeadInSlots = Set.empty -- never a leader, FIXME
              , cfgsp_securityParam= SecurityParam{maxRollbacks= 1}
              }

---- Trivial Block (for the SP protocol) -------------------------------------
--
--   see 4.3 in [CCASL]
--
--   borrowing from
--    - https://iohk.io/en/blog/posts/2020/05/28/the-abstract-nature-of-the-consensus-layer/
--        which references 'MiniConsensus.hs'

--    - ouroboros-consensus-test/test-consensus/Test/Consensus/HardFork/Combinator/A.hs & B.hs

-- | Map the block to a consensus protocol
type instance BlockProtocol TrivBlock = SP

-- | Define TrivBlock
data TrivBlock = TrivBlock
                   { tb_header :: Header TrivBlock
                   }
  deriving NoThunks via OnlyCheckWhnfNamed "TrivBlock" TrivBlock

  -- TrivBlock has no tx's, nothing but a header

data instance Header TrivBlock =
  HdrTB
    { htb_SlotNo  :: SlotNo
    , htb_BlockNo :: BlockNo
    , htb_Hash    :: HeaderHash TrivBlock
    , htb_prev    :: ChainHash TrivBlock
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrTB" (Header TrivBlock)


instance BlockSupportsProtocol TrivBlock where
  validateView _ _ = ()
  -- selectView   = stub
  -- this method defaulted.  (MT-TODO: understand.)
  -- TODO: do we want to use the defalt method in some/all of our pills?

instance GetHeader TrivBlock where
  getHeader          = tb_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB        = const Nothing

instance GetPrevHash TrivBlock where
  headerPrevHash = htb_prev

instance HasHeader TrivBlock where
  getHeaderFields = castHeaderFields       -- worth some commentary?
                  . getHeaderFields
                  . tb_header
                    
instance HasHeader (Header TrivBlock) where
  getHeaderFields hdr = HeaderFields
                          { headerFieldSlot   = htb_SlotNo hdr
                          , headerFieldBlockNo= htb_BlockNo hdr
                          , headerFieldHash   = htb_Hash hdr
                          }

instance StandardHash TrivBlock
  
type instance HeaderHash TrivBlock = Hash

type Hash = String  -- FIXME: any need to get more complicated?

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
trivBlock = TrivBlock { tb_header= HdrTB stub stub stub stub -- TODO
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

