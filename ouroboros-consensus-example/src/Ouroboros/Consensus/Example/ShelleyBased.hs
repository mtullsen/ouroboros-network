{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Example.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (PraosCrypto)
import           Ouroboros.Consensus.Shelley.ShelleyBased

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c. PraosCrypto c
  => (   forall era. (EraCrypto era ~ c, ShelleyBasedEra era)
      => LedgerState (ShelleyBlock era)
      -> LedgerState (ShelleyBlock era)
     )
  -> LedgerState (ExampleBlock c)
  -> LedgerState (ExampleBlock c)
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (ExampleEras c)
    fs = injectShelleyNP
           reassoc
           (hcpure
             (Proxy @(And (HasCrypto c) ShelleyBasedEra))
             (fn (Comp . f . unComp)))

    reassoc ::
         (     LedgerState :.: ShelleyBlock
          -.-> LedgerState :.: ShelleyBlock
         ) shelleyEra
      -> (     LedgerState
          -.-> LedgerState
         ) (ShelleyBlock shelleyEra)
    reassoc g = fn $ unComp . apFn g . Comp
