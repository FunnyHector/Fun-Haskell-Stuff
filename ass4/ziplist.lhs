==============================================
                      Code
==============================================


> module ZipList where

Type declaration:

> newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

First define ZipList as a Functor.

> instance Functor ZipList where
>   fmap f (ZipList xs) = ZipList (map f xs)

Then define it as Applicative Functor.

> instance Applicative ZipList where
>   pure x = ZipList (repeat x)
>   ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


==============================================
                   Discussion
==============================================


1. Why I take this approach?
   Discuss the choices I made in building applicative functor





2. why ZipList is not suitable as a Monad?

If we want to try make a Monad ZipList instance, we'll have to build it on top
of current definition of Applicative ZipList, which means:

    return x = pure x = ZipList (repeat x)

in plain English, each time we lift a normal value x into its monadic context,
it becomes a infinite list of itself. Given this premise, let's try to implement
the (>>=) function to satisfy monad laws.

For the left identity law (return a >>= f == f a), if we could somehow satisfy:

    ZipList (repeat a) >>= f  ==  f a

But wait, the left hand side will be infinite, but we can never guarantee that
the right hand side (f a) will be infinite given that f is unknown.

I made several attempts but couldn't even make it satisfy the first law, left
identity. I found a few discussions (on Haskell-Cafe mailing list, Haskell on
Reddit, and Quora) about this topic, but none of the solutions brought out in
those discussions satisfy all three monad laws.

So while the conclusion is clear that ZipList is not suitable as a monad, the
reason, if to guess, is that we have to make `pure` function to return an
infinite list to deal with two lists with different size, which makes (>>=)
impossible to satisfy monad laws.


3. option todo:
it could be a monad if the length was fixed. FixedZipList { [], size }
