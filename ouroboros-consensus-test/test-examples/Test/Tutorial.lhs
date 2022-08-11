
% Example: Implementing a Simple Protocol Using `ouroborus-consensus`

Introduction and Motivation
===========================

This example is a compilable Literate Haskell (`.lhs`) file that
instantiates the `ConsensusProtocol` typeclass to serve as an
example of some of the high-level concepts in `ouroboros-consensus`

**TODO: More**

This example uses several extensions:

> {-# LANGUAGE TypeFamilies       #-}
> {-# LANGUAGE DerivingVia        #-}
> {-# LANGUAGE DataKinds          #-}
> {-# LANGUAGE DeriveGeneric      #-}
> {-# LANGUAGE FlexibleInstances  #-}
> {-# LANGUAGE DeriveAnyClass     #-}
> {-# LANGUAGE StandaloneDeriving #-}
> module Test.Tutorial() where

First, some includes we'll need:

> import Data.Void(Void)
> import Data.Set(Set)
> import qualified Data.Set as Set
> import Data.Word(Word64)
> import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
> import GHC.Generics (Generic)
> import Codec.Serialise (Serialise)
> import Ouroboros.Consensus.Block.Abstract
> import Ouroboros.Consensus.Protocol.Abstract
> import Ouroboros.Consensus.Ticked
> import Ouroboros.Consensus.Block (BlockSupportsProtocol (selectView, validateView))
> import Ouroboros.Consensus.Ledger.Abstract
>
> import Test.Utilities (Hash)
> import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)


The `ConsensusProtocol` typeclass
=================================

The central abstraction of `ouroborus-consensus` is the `ConsensusProtocol`
typeclass.  This class captures the relationship between consensus and the
rest of the system (in particular the ledger) as a set of type families.

To demonstrate these relationships, we will begin by defining a simple
protocol creatively named `SP`.

First, we define the type of the protocol itself.  This is a type-level "tag", this does not exist
at the value level.

> data SP

The static configuration for `SP` is defined by defining an instance for the
`ConsensusConfig` type family.  Some of the methods in `ConsensusProtocol` class such as
`checkIsLeader` require an associated `ConsensusConfig p` so we define a simple one here:

> data instance ConsensusConfig SP =
>  SP_Config  { cfgsp_slotsLedByMe :: Set SlotNo
>             }

Next, we instantiate the `ConsensusProtocol` for `SP`:

> instance ConsensusProtocol SP where
>   type SelectView    SP = BlockNo
>
>   type LedgerView    SP = ()
>
>   type IsLeader      SP = SP_IsLeader
>   type CanBeLeader   SP = SP_CanBeLeader
>
>   type ChainDepState SP = ()
>   type ValidateView  SP = ()
>   type ValidationErr SP = Void
>
>   checkIsLeader cfg SP_CanBeLeader slot _tcds =
>       if slot `Set.member` cfgsp_slotsLedByMe cfg
>       then Just SP_IsLeader
>       else Nothing
>
>   protocolSecurityParam _cfg = k
>
>   tickChainDepState     _ _ _ _ = TickedTrivial
>
>   updateChainDepState   _ _ _ _ = return ()
>
>   reupdateChainDepState _ _ _ _ = ()

Finally we define a few extra things used in this instantiation:

**TODO: more detail**

> data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
> data SP_IsLeader    = SP_IsLeader    -- Evidence that we /are/ leader>
>
> k :: SecurityParam
> k = SecurityParam {maxRollbacks= 0}
>  -- FIXME: k=0 is degenerate (results in much dead code), 1 is interesting.

Let's examine each of these in turn:

Chain Selection: `SelectView`
-----------------------------

One of the major decisions when implementing a consensus protocol is encoding a
policy for chain selection.  The `SelectView SP` type represents the information
necessary from a block header to help make this decision.

The other half of this - which explains how a `SelectView` is derived from
a particular block - is expressed by the block's implementation of the
 `BlockSupportsProtocol` typeclass.  (See pill N.) **:FIXME**

The `preferCandidate` function in `Ouroboros.Consensus.Protocol.Abstract`
demonstrates how this is used.

Note that instantiations of `ConsensusProtocol` for some protocol `p`
consequently require `Ord (SelectView p)`.

For `SP` we will use only `BlockNo` - to implement the simplest
rule of preferring longer chains to shorter chains.


Ledger Integration: `LedgerView`
--------------------------------

Some decisions that a consensus protocol needs to make will depend
on the ledger's state, `LedgerState blk`.  The data required from the ledger
is of type `LedgerView p` (i.e., the protocol determines what is needed).
Similar to `SelectView` the projection of `LedgerState blk` into `LedgerView p` exists
in a typeclass, namely `LedgerSupportsProtocol`.


**TODO: there's some subtlety here wrt `LedgerView` and prediction that might
be worth discussing.**

For `SP` we do not require any information from the ledger to make
decisions of any kind.  In the Praos protocol, the `LedgerView`
contains information about the stake distribution among other things.

Notably, this is used in the `tickChainDepState` function elsewhere in the
`ConsensusProtocol`.


Protocol State: `ChainDepState`, `ValidateView` and `ValidationErr`
----------------------------------------------------------------

`ChainDepState` describes the state of the protocol that evolves with the chain.
Note, from [Cardano Consensus and Storage Layer]: ``we are referring to this as
the “chain dependent state” to emphasize that this is state that evolves with
the chain, and indeed is subject to rollback when we switch to alternatives
forks. This distinguishes it from chain independent state such as evolving
private keys, which are updated independently from blocks and are not subject to
rollback.''

`ValidateView` is a 'view' of a block (header) providing enough information to validate
the block header.
It is called `ValidateView` because the functions used to
compute new states from some combination of a prior `ChainDepState`
and a `ValidateView` can _fail_ - producing a `ValidationErr`.

There are some interesting constraints governing what can appropriately
be used as a type fulfilling the requirements of `ValidateView` - in
particular the fact that `ConsensusProtocol` instances are sometimes called
upon to do _prediction_ rather than just as a pure summary of history - and
as such may not be able to witness a chain in its entirety.

For more details, see the definition of `ConsensusProtocol`.


Protocol State: `tickChainDepState`, `updateChainDepState` and `reupdateChainDepState`
-----------------------------------------------------------------------------------

These three functions model state transitions of values of type `ChainDepState`

`tickChainDepState` computes a new `ChainDepState` from a prior state though
a computation that models the (logical) passage of time.
**TODO: more about `Ticked`** **:(MT) how about in pill-N**
Unlike `updateChainDepState` this cannot fail under normal circumstances - if
it could, that would mean there is some failure that is inevitable given
the passage of time and if that is the case there would have been no reason
not to throw such an error immediately.

`updateChainDepState` (a better name would be "applyHeader") computes a new `ChainDepState` from a prior state and
the needed view of the header, `ValidateView p`.  This could fail, producing a
`ValidationErr p` instead of a `ChainDepState p`

`reupdateChainDepState` is an optimization of `updateChainDepState` which
is called when the header is known to be good (e.g., from a previous call to `updateChainDepState`)
and the header check is unneeded.

**TODO: explain what SP's implementation means**

Leader Selection: `IsLeader`, `CanBeLeader`, `checkIsLeader`
------------------------------------------------------------

**TODO: more about leadership conceptually somewhere?**
**TODO: is leadership used anywhere else?**

The type family `CanBeLeader` represents the ability for a particular node
in the protocol to be a leader for a slot.  Put another way, a value of `CanBeLeader p` for a particular `p`
witnesses the ability to be a leader in a particular context.

In the same way, a value `IsLeader` witnesses the fact that a particular node is a leader for a slot.

In `SP` both of these are simple singleton types but in more complex protocols
they may contain cryptographic proof of the property witnessed by each.

The `checkIsLeader` function uses these types in its determination of whether or not a node is a leader
for a slot - returning `Nothing` if the node is not a slot leader or `Just (IsLeader p)`
if it is.

`SP` implements leadership by specifying, in the static `ProtocolConfig` for `SP`,
a set of slots for which the particular node running the protocol is the leader.  `checkIsLeader`
then looks up the slot number in this set and returns `Just SP_IsLeader` (aka `IsLeader SP`) if
the node is configured to be a leader in this slot.


The Security Parameter `k`: `protocolSecurityParam`
---------------------------------------------------

`ConsensusProtocol` requires that its static configuration --
which is to say the associated `ConsensusConfig p` for a particular
`ConsensusProtocol p` -- provide a security parameter (`SecurityParam`).
This requirement is embodied in the `protocolSecurityParam` method.

For all known/current protocols, the security parameter is fixed for each
blockchain (a protocol could be instantiated with different k's, but it should
be configured the same for each node in that blockchain).

The `maxRollbacks` field on the `SecurityParam` record (often referred to as `k`)
describes how many blocks can be rolled back - any number of blocks greater than
this should be considered permanently part of the chain with respect to the protocol.

**TODO: what does this affect?  chain selection?** **MT: absolutely, :-)**

In the case of `SP` we don't allow rollback at all.



Further reading about Consensus
-------------------------------

The `ConsensusProtocol` class is also dealt with in some detail
and with additional context in the
[Cardano Consensus and Storage Layer](https://hydra.iohk.io/build/15874054/download/1/report.pdf) report.

The `Ouroboros.Consensus.Protocol.Praos` module contains the
instantiation of `ConsensusProtocol` for Praos.




Blocks: The View From Consensus
===============================

In the discussion above, the reader may have noticed that we have only
presented _views_ of some of the things consensus deals with.  This is
to reduce coupling between `ConsensusProtocol p` and any particular
block or ledger implementation.

**TODO: more intro material**

To enhance our example we'll implement a simple block
and ledger that can be used with `SP` that logically keeps track of a
single number.  Each block contains a list of transactions that either
increment or decrement the number and at any point in time, and the
ledger's state can be thought of the net effect of all these
transactions - in other words, the number of increment transactions
minus the number of decrement transactions.

Defining the Block
------------------

We'll start by defining the transaction type - this is what the block
will contain:

> data Tx = Inc | Dec
>           deriving (Show, Eq, Generic, Serialise)

Next, we'll define the block itself:

> data BlockC = BlockC { bc_header :: Header BlockC
>                      , bc_body   :: [Tx]
>                      }

Which is to say, a block is just a header (`Header BlockC`) followed by a
list of transactions (`[Tx]`) - we'll need to instantiate the
data families `GenTx` and `Header` for `BlockC`.

**TODO: What is `GenTx`?**

We'll deal with `Header BlockC` in the next section.

Block Headers
-------------

The block header describes the _structure_ of the block chain - for example
the hash of this block and that of the block before it. (**TODO: what about genesis?**)
This corresponds to the `Header` data family (from `Ouroboros.Consensus.Block.Abstract`)
which we'll instantiate as:

> data instance Header BlockC =
>   HdrBlockC
>     { hbc_SlotNo  :: SlotNo
>     , hbc_BlockNo :: BlockNo
>     , hbc_Hash    :: HeaderHash BlockC
>     , hbc_prev    :: ChainHash BlockC
>     }
>   deriving stock    (Show, Eq, Generic)
>   deriving anyclass (Serialise)

Because `Header` is a data family, functions using instantiations of this
family will know nothing about the structure of the data - instead there
are other typeclasses needed to build an interface to derive things that
are needed from this value.  We'll implement those typeclasses next.

Interface to the Block Header
-----------------------------

** `GetHeader` **

The `GetHeader` class **TODO: where does it live?**
describes how to project a header - a value of type `Header BlockC`
in our example - out of a block representation.  The implementation
for `getHeader` is fairly straightforward - we can just use the record
accessor `bc_header`:

> instance GetHeader BlockC where
>    getHeader          = bc_header
>    blockMatchesHeader = \_ _ -> True -- TODO: actually implement this function
>    headerIsEBB        = const Nothing


** `GetPrevHash` **

The `GetPrevHash` class contains a function that gets the hash of a
previous block from the header - which is very simple for `Header BlockC`:

> instance GetPrevHash BlockC where
>  headerPrevHash = hbc_prev

** `HasHeader` **

The `HasHeader` typeclass has the `getHeaderFields` function which projects the
information in the header to a `HeaderFields` record containing the slot, block number, and
block hash.

We implement this both for `Header Block`:

> instance HasHeader (Header BlockC) where
>   getHeaderFields hdr = HeaderFields
>                          { headerFieldSlot    = hbc_SlotNo hdr
>                          , headerFieldBlockNo = hbc_BlockNo hdr
>                          , headerFieldHash    = hbc_Hash hdr
>                          }

As well as `BlockC` itself - which calls the `getHeaderFields` defined for `Header BlockC`:

> instance HasHeader BlockC where
>   getHeaderFields = castHeaderFields
>                   . getHeaderFields
>                   . bc_header

**TODO: what are these **

> type instance HeaderHash BlockC = Hash

> instance StandardHash BlockC

Associating the Block and the Protocol
--------------------------------------

So far, we've made no mention of `SP` in any of the definitions for `BlockC` -
similarly, we've made no mention of `BlockC` in any of the definitions
for `SP` we have to implement a few more typeclasses that define
how the two are associated.

More generally, a block has one and only one type of protocol - but the converse
is not true - a protocol may have many types of block.  As such, the association
between the two specifies the protocol for a particular type of block.
The type family establishing this relationship is the `BlockProtocol` class.

Here, we define the protocol type for `BlockC` as `SP`:

> type instance BlockProtocol BlockC = SP

Also, the other half of `ValidateView SP` needs to be defined as well -
which is how do we create a value of `ValidateView SP` given a block.  To
do this, we instantiate the `BlockSupportsProtocol` typeclass.  Note that
we do not need to say _which_ protocol is supported since there is only
ever one protocol for a block, again established by our prior instantiation of
 `BlockProtocol`:

> instance BlockSupportsProtocol BlockC where
>   validateView _ _ = ()
>   selectView   _bcfg hdr  = blockNo hdr

Given that `ValidateView SP` is of type `()` there is only one possible implementation
for this typeclass.  Later examples will require more interesting views of the block.

** TODO: Describe these configuration classes **

> data instance BlockConfig BlockC = BCfgBlockC
>   deriving (Generic, NoThunks)
> data instance CodecConfig BlockC = CCfgBlockC
>   deriving (Generic, NoThunks)
> data instance StorageConfig BlockC = SCfgBlockC
>   deriving (Generic, NoThunks)


Consensus and The Ledger
========================

The _ledger_ specifies a state of the system represented by the blocks
in a blockchain but also characterizes what transitions are valid for
any particular state.

**TODO: more explanation here**


Defining the Ledger
-------------------

Much like `ConsensusProtocol` and its `ConsensusConfig` configuration class,
the ledger has an associated static configuration which is represented using
the type family `LedgerCfg`.  For our example, we have nothing
interesting to configure, thus:

> type instance LedgerCfg (LedgerState BlockC) = ()

Given that the `BlockC` transactions consist of incrementing and decrementing
a number, we materialize that number in the `LedgerState`.  However,
we also need to have some notion of _where_ in the blockchain we are
representing with this number.  Our instantiation of `LedgerState` for
`BlockC` includes both these things.

> data instance LedgerState BlockC =
>   LedgerC
>      { lsbc_tip   :: Point BlockC -- header hash and slot num.
>      , lsbc_count :: Word64       -- results of an up/down counter
>      }
>    deriving (Show, Eq, Generic, Serialise)

The `Point` type (defined in `Ouroboros.Network.Block`) describes a particular
place in the blockchain - a combination of a slot and a block hash.

**TODO: explain exactly what a slot is at this point?**

**TODO: how is a slot given meaning?**

**TODO: `Ticked` refers to a logical clock of some kind - what exactly is this clock in the ledger's case?**

> newtype instance Ticked (LedgerState BlockC) =
>   TickedLedgerStateC {
>     unTickedLedgerStateC :: LedgerState BlockC
>   }
>   deriving (Show, Eq, Generic, Serialise)

> type instance ApplyTxErr BlockC = ()


Appendix: NoThunks Instances
============================

**TODO: what is NoThunks and why is it everywhere?**

To focus on the salient ideas of this document, we've put all the derivations
of `NoThunks` here instead:

> deriving via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP) instance NoThunks (ConsensusConfig SP)
> deriving via OnlyCheckWhnfNamed "BlockC" BlockC instance NoThunks BlockC
> deriving via OnlyCheckWhnfNamed "HdrBlockC" (Header BlockC) instance NoThunks (Header BlockC)
> deriving via OnlyCheckWhnfNamed "LedgerC" (LedgerState BlockC) instance NoThunks (LedgerState BlockC)
> deriving instance NoThunks (Ticked (LedgerState BlockC))
