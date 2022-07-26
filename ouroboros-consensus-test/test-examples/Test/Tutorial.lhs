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

The central abstraction of @ouroborus-consensus@ is the `ConsensusProtocol`
typeclass.  This class captures the relationship between consensus and the
rest of the system (in particular the ledger) as a set of type families.

To demonstrate these relationships, we will begin by defining a simple
protocol creatively named `SP`.

First, we define the type of the protocol itself.  As we will see,
it is perfectly acceptable for this to be an empty type, since
functions in the `ConsensusProtocol` class only refer to various
type families that are declared as part of `ConsensusProtocol` itself.

> data SP

The static configuration for `SP`` is defined by instantiating the
`ConsensusConfig` class.  Some of the functions in `ConsensusConfig` such as
`checkAsLeader` require an associated `ConsensusConfig` so we define a simple one here:

> data instance ConsensusConfig SP =
>  SP_Config  { cfgsp_iLeadInSlots :: Set SlotNo
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
>       if slot `Set.member` cfgsp_iLeadInSlots cfg
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
policy for chain selection.  The `SelectView` type represents the information
necessary from the ledger by the consensus protocol to help make this decision.
The `preferCandidate` function in `Ouroboros.Consensus.Protocol.Abstract`
demonstrates how this is used.

Note that instantiations of `ConsensusProtocol` for some type `T`
consequently require `Ord (SelectView T)`.

For `SP` we will use only `BlockNo` - to implement the simplest
rule of perferring longer chains to shorter chains.


Ledger Integration: `LedgerView`
--------------------------------

Some decisions that a consensus protocol needs to make will depend
on the ledger.  This dependency is expressed via `LedgerView`.

For `SP` we do not require any information from the ledger to make
decisions of any kind.  In the Praos protocol, the `LedgerView`
contains information about the stake distribution among other things.

Notably, this is used in the `tickChainDepState` function elsewhere in the
`ConsensusProtocol`.


Protocol State: `ChainDepState`, `ValidateView` and `ValidationErr`
----------------------------------------------------------------

`ChainDepState` describes a logical state that summarizes a chain.

`ValidateView` is a 'view' of a block providing enough information to update
the state.  It is called `ValidateView` because the functions used to
compute new states from some combination of a prior `ChainDepState`
and a `ValidateView` can _fail_ - producing a `ValidationErr`.

There are some interesting constraints governing what can appropriately
be used as a type fulfilling the requirements of `ValidateView` - in
particular the fact that `ConsensusProtocol` instances are sometimes called
upon to do _prediction_ rather than just as a pure summary of history - and
as such may not be able to witness a chain in its entirety.

For more details, see the definition of `ConsensusProtocol`


Protocol State: `tickChainDepState`, `updateChainDepState` and `reupdateChainDepState`
-----------------------------------------------------------------------------------

These three functions model state transitions of values of type `ChainDepState`

`tickChainDepState` computes a new `ChainDepState` from a prior state though
a computation that models the (logical) passage of time.  **TODO: more about `Ticked`**
Unlike `updateChainDepState` this cannot fail under normal circumstances - if
it could, that would mean there is some failure that is inevitable given
the passage of time and if that is the case there would have been no reason
not to throw such an error immediately.

`updateChainDepState` computes a new `ChainDepState` from a prior state and
a `ValidationView` -- essentially a new block.  This could fail, producing a
`ValidationErr` instead of a `ChainDepState`

`reupdateChainDepState` is a special case of `updateChainDepState` which
is used when the update is not expected to fail -- one example might be when it has
been previously validated by calls to `updateChainDepState`

**TODO: explain why what SP's implementation means**

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

`ConsensusProtocol` requires that its static configration --
which is to say the associated `ConsensusConfig p` for a particular
`ConsensusProtocol p` -- provide a security parameter (`SecurityParam`).
This requirement is embodied in the `protocolSecurityParam` function.

The `maxRollbacks` field on the `SecurityParam` record (often referred to as `k`)
describes how many blocks can be rolled back - any number of blocks greater than
this should be considered permanently part of the chain with respect to the protocol.

**TODO: what does this affect?  chain selection?**

In the case of `SP` we don't allow rollback at all.


Further reading
---------------

The `ConsensusProtocol` class is also dealt with in some detail
and with additional context in the
[Cardano Consenus and Storage Layer](https://hydra.iohk.io/build/15874054/download/1/report.pdf) report.

The `Ouroboros.Consensus.Protocol.Praos` module contains the
instantiation of `ConsensusProtocol` for Praos.


