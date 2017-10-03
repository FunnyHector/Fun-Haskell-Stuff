General discussion:




============================================
                  Code
============================================

> module Box (
>   Box(..)
>
> ) where

> import Control.Monad

Type declaration:

> newtype Box a = Box a deriving (Show, Eq)

To be a Monad, Box must be a Functor first.

> instance Functor Box where
>   fmap f (Box a) = Box (f a)

Box also must be an Applicative Functor.

> instance Applicative Box where
>   pure = return
>   (<*>) = ap

Now Box is ready to be a Monad.

> instance Monad Box where
>   return = Box
>   (Box a) >>= f = f a

Box obeys monad laws.

For left identity law:
  return a >>= f    ==    f a
     Box a >>= f    ==    f a
             f a    ==    f a    -- Proved

For right identity law:
  (Box a) >>= return    ==    Box a
            return a    ==    Box a
               Box a    ==    Box a    -- Proved

For associativity:
  ((Box a) >>= f) >>= g    ==    (Box a) >>= (\x -> f x >>= g)
              f a >>= g    ==    (\x -> f x >>= g) a
              f a >>= g    ==    f a >>= g    -- Proved
