% Example: Implementing a Simple Protocol Using `ouroborus-consensus`

Introduction and Motivation
===========================

This example is a compilable Literate Haskell (`.lhs`) file that
instantiates the `ConsensusProtocol` typeclass to serve as an
example of some of the high-level concepts in `ouroborus-consensus`

**TODO: More**

This example uses several extensions:

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE DerivingVia  #-}
> {-# LANGUAGE DataKinds    #-}
> module Test.Tutorial() where

First, some includes we'll need:

> import Data.Void(Void)
> import Data.Set(Set)
> import qualified Data.Set as Set
> import Ouroboros.Consensus.Block.Abstract
> import Ouroboros.Consensus.Protocol.Abstract
> import Ouroboros.Consensus.Ticked
> import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))


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
>             deriving NoThunks via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP)

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

Let's examine each of these in turn:

Chain Selection: `SelectView`
-----------------------------

One of the major decisions when implementing a consenus protocol is encoding a
policy for chain selection.  The `SelectView SP` type represents the information
necessary from a block header to help make this decision.

The other half of this - which explains how a `SelectView` is derived from
a particular block - is expressed by the block's implementation of the
 `BlockSupportsProtocol` typeclass.

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
the “chain dependent state” to emphasise that this is state that evolves with
the chain, and indeed is subjec to rollback when we switch to alternatives
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

Although the security parameter does appear to be constant for all current
protocols, the fact that it is read from the `ConsensusConfig p` allows it to
change for testing purposes.

The `maxRollbacks` field on the `SecurityParam` record (often referred to as `k`)
describes how many blocks can be rolled back - any number of blocks greater than
this should be considered permanently part of the chain with respect to the protocol.

**TODO: what does this affect?  chain selection?** **MT: absolutely, :-)**

In the case of `SP` we don't allow rollback at all.



Further reading
---------------

The `ConsensusProtocol` class is also dealt with in some detail
and with additional context in the
[Cardano Consensus and Storage Layer](https://hydra.iohk.io/build/15874054/download/1/report.pdf) report.

The `Ouroboros.Consensus.Protocol.Praos` module contains the
instantiation of `ConsensusProtocol` for Praos.


