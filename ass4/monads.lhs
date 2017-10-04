> module Box where

> import Control.Monad

Type declaration:

> newtype Box a = Box a deriving (Show, Eq)

To be a Monad, Box must be a Functor first.

> instance Functor Box where
>   fmap f (Box a) = Box (f a)

Box also must be an Applicative Functor.

> instance Applicative Box where
>   pure  = Box
>   (<*>) = ap

Now Box is ready to be a Monad.

> instance Monad Box where
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

The following simple test function is a simple example of left identity law
(though it is using a general monad instead of Box monad). It should give us
True.

> test1 :: Bool
> test1 = (return 5 >>= \x -> show x) == show 5

The following simple test function is a simple example of right identity law.
It should give us True.

> test2 :: Bool
> test2 = (Box 5 >>= return) == Box 5

The following simple test function is a simple example of associativity law.
It should give us True.

> test3 :: Bool
> test3 = ((Box 5 >>= f) >>= g) == (Box 5 >>= (\x -> f x >>= g))
>   where f x = Box ((+5) x)
>         g x = Box (show x)
