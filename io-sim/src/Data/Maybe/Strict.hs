module Data.Maybe.Strict where

import           Control.Monad (ap)

data SMaybe a = SJust !a
              | SNothing
  deriving (Eq, Ord, Show)


instance Functor SMaybe where
    fmap f (SJust a) = SJust (f a)
    fmap _ SNothing  = SNothing

instance Applicative SMaybe where
    (<*>) = ap
    pure  = SJust

instance Monad SMaybe where
    SNothing >>= _ = SNothing
    SJust a  >>= f = f a

    return = pure

instance Foldable SMaybe where
    foldMap f (SJust a) = f a
    foldMap _ SNothing  = mempty


fromMaybe :: a -> SMaybe a -> a
fromMaybe _ (SJust a) = a
fromMaybe a  SNothing = a

toLazy :: SMaybe a -> Maybe a
toLazy (SJust a) = Just a
toLazy SNothing  = Nothing
{-# INLINE toLazy #-}

fromLazy :: Maybe a -> SMaybe a
fromLazy (Just a) = SJust a
fromLazy Nothing  = SNothing
{-# INLINE fromLazy #-}


maybe :: b -> (a -> b) -> SMaybe a -> b
maybe _ f (SJust a) = f a
maybe b _  SNothing = b
