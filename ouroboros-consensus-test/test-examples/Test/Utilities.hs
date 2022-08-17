{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Test.Utilities where

-- base pkgs:
import           GHC.Generics (Generic)

-- pkg nothunks:
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

-- pkg hashable:
import           Data.Hashable

-- pkg serialise:
import           Codec.Serialise


newtype Hash = Hash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (NoThunks, Hashable, Serialise)

hash' :: Hashable a => a -> Hash
hash' = Hash . hash

tickStub :: a
tickStub = error "tickstub"
  -- b/c I don't yet know how to tick things, :-)

trivTick :: a -> a
trivTick a = a

  
stub :: a
stub = error "stub"
