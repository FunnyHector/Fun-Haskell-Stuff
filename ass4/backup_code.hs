module Redundant where


data LockableBox a = LockedBox a | UnlockedBox a deriving (Show, Eq)


instance Functor LockableBox where
  fmap f (UnlockedBox x) = UnlockedBox (f x)
  fmap _ (LockedBox x)   = LockedBox x


instance Applicative LockableBox where
  pure = UnlockedBox
  -- (<*>) =


instance Monad LockableBox where
