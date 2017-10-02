
> module Box (
>   Box(..)
>
> ) where

> import Control.Monad

> newtype Box a = Box a deriving (Show, Eq)

> instance Monad Box where
>   return = Box
>   (Box a) >>= f = f a

> instance Functor Box where
>   fmap f (Box a) = Box (f a)

> instance Applicative Box where
>   pure = return
>   (<*>) = ap
