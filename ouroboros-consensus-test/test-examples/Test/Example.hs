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

---- tests -------------------------------------------------------------------
-- TODO: put something interesting in here.

test1 :: TestTree
test1 = testProperty "prop_example1" prop_example1

prop_example1 :: Bool
prop_example1 = True


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
  -- 
  -- Q. whatsup with last three methods?
  -- A. see next
  
{-

 Notes from Nick wrt class ConsensusProtocol:

  Fundamental idea here
    - header/block split

 Three concepts:
   1. 
    chaindepstate = state you need as validating headers, state you can
    update as you get headers (w/o having [interleaving] blocks)

     (related to the header/block split concept!)

   2. concept: ticking.
     - can get chaindep state for _
     - moving through time: tick
     - ledger state in slot, then tick, remember slot we started from.
       - ticked info: this is data we need, but not in the original _
   
   3. update vs reupdate
     - reupdateChainDepState
        - note: doesn't check header, no fail; call when header known good.

       -- | Apply a header
       updateChainDepState :: HasCallStack
                           => ConsensusConfig       p
                           -> ValidateView          p  -- this from header
                           -> SlotNo
                           -> Ticked (ChainDepState p) -- we had to tick to get her
                           -> Except (ValidationErr p) (ChainDepState p)
            -- before and after.
            -- better name: apply header
  
-}

k :: SecurityParam
k = SecurityParam {maxRollbacks= 0}
  -- Q. any reason we need to put into ConsensusConfig?
  -- Q. not generally a system constant?
  --   NF: k be different for diff nodes?  Probably not: A property of the chain!
  --   ConsensusConfig and the node's config file: generally 1-1.
  --    - but maybe ConsensusConfig is also serving as "global variable container"?
  --    - BTW, look at logic to build ConsensusConfig.

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
  -- Q. A block cannot be used in multiple protocols? No. Ultimately block determines protocol.

-- | Define TrivBlock
data TrivBlock =
    TrivBlock
      { tb_header :: Header TrivBlock
      , tb_body   :: [GenTx TrivBlock]  -- NF: remove for first.
      -- , tb_Epoch  :: EpochNo
      }
  deriving NoThunks via OnlyCheckWhnfNamed "TrivBlock" TrivBlock

data instance Header TrivBlock = HdrTB {
      hdrTB_fields :: HeaderFields TrivBlock -- NF: inline! make simpler.
    , hdrTB_prev   :: ChainHash    TrivBlock
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "HdrTB" (Header TrivBlock)


-- NOTE: BlockSupportsProtocol has *many* superclasses.


-- | evidence TrivBlock supports (BlockProtocol TrivBlock), the methods:

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


{- TODO: Turn this into text/explanation.

(BTW, be nice to have a class hierarchy diagram.)

Q. this feels ~ awkward, motivation/explanation?

  class HasHeader (Header blk) ⇒ GetHeader blk where ...

  class (HasHeader blk, GetHeader blk) ⇒ GetPrevHash blk where

  -- NF: just to overload 'getHeader': works on block/header.

Q. motivation/explanation?

  both TrivBlock and (Header TrivBlock) in 'HasHeader' ?

-}


instance HasHeader TrivBlock where
  getHeaderFields = getBlockHeaderFields
                    -- see doc in *.Block.Abstract ; ~complex
                    -- getBlockHeaderFields - seems to rely on ...

                    
instance HasHeader (Header TrivBlock) where
  getHeaderFields = castHeaderFields . hdrTB_fields
  
  -- NOTE: Header is type-constructor, not type-family!
  --  - hard to keep these things straight.
  
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
        
      -- TODO: get rid of this:
      -- | The 'SlotNo' of the block containing the 'InitiateAtoB' transaction
    , lgrA_transition :: Maybe SlotNo
    }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerA" (LedgerState TrivBlock)

  -- BTW: A&B were testing the transition.

data instance GenTx TrivBlock = TxA { txName :: String }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxA" (GenTx TrivBlock)

-- triv block has no tx's.

---- data --------------------------------------------------------------------

trivBlock :: TrivBlock
trivBlock = TrivBlock { tb_header= HdrTB stub stub -- TODO
                      , tb_body  = [TxA "tx1", TxA "tx2"]
                      }

---- library -----------------------------------------------------------------

stub :: a
stub = error "stub"
