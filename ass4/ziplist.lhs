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
>   ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)
>   pure x = ZipList (repeat x)


==============================================
                   Discussion
==============================================


1.  Why this approach?
- to implement ZipList as an Applicative instance, we need to implement two
functions: `(<*>)` and `pure`.

`(<*>)` is rather easy. ZipList behaves like the Prelude function `zipWith`,
so this is a straightforward approach:

  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

`pure` seems to be easy too. ZipList is essentially a list, so:

  pure x = ZipList [x]

However, when we check the applicative functor laws, they don't hold the
identity law:

         pure id <*> ZipList xs    ==    ZipList xs
    ZipList [id] <*> ZipList xs    ==    ZipList xs
  ZipList (zipWith ($) [id] xs)    ==    ZipList xs
              ZipList [head xs]    ==    ZipList xs    -- incorrect!

The problem comes from the use of function `zipWith`: if either list is too
short to match up with the other, only the matching elements are used. In other
words, `zipWith` will cut off all unmatched elements in the longer list. So if
what `pure` does is to simply wrap the value into Applicative Functor,
`pure id <*> v` only preserves the first element in `v`. To have all elements
preserved, `pure id` has to produce `ZipList [id, id, ...]`, which is:

  pure x = ZipList (repeat x)

Now we test whether all applicative laws are satisfied.

(1) For Identity law:

                pure id <*> ZipList xs    ==    ZipList xs
    ZipList (repeat id) <*> ZipList xs    ==    ZipList xs
  ZipList (zipWith ($) (repeat id) xs)    ==    ZipList xs
                            ZipList xs    ==    ZipList xs    -- satisfied!

(2) For Homomorphism law:

                            pure f <*> pure x    ==    pure (f x)
    ZipList (repeat f) <*> ZipList (repeat x)    ==    ZipList (repeat (f x))
  ZipList (zipWith ($) (repeat f) (repeat x))    ==    ZipList (repeat (f x))
                       ZipList (repeat (f x))    ==    ZipList (repeat (f x))    -- satisfied!

(3) For Interchange law:

                ZipList fs <*> pure y    ==    pure ($ y) <*> ZipList fs
    ZipList fs <*> ZipList (repeat y)    ==    ZipList (repeat ($ y)) <*> ZipList fs
  ZipList (zipWith ($) fs (repeat y))    ==    ZipList (zipWith ($) (repeat ($ y)) fs)
  ZipList (zipWith ($) fs (repeat y))    ==    ZipList (zipWith ($) (repeat ($ y)) fs)

now since we have:

  ($) f y = f y, and
  ($) ($ y) f = ($ y) $ f = f y,

so we can prove:

  ZipList (zipWith ($) fs (repeat y))    ==    ZipList (zipWith ($) (repeat ($ y)) fs)    -- satisfied!

(4) For Composition law:

              pure (.) <*> ZipList us <*> ZipList vs <*> ZipList ws    ==    ZipList us <*> (ZipList vs <*> ZipList ws)
  ZipList (repeat (.)) <*> ZipList us <*> ZipList vs <*> ZipList ws    ==    ZipList us <*> (ZipList vs <*> ZipList ws)

For each list in ZipList, i.e. us, vs, and ws, we can safely assume they all
have only one element to simplify the provement:

  ZipList [(.)] <*> ZipList [u] <*> ZipList [v] <*> ZipList [w]    ==    ZipList [u] <*> (ZipList [v] <*> ZipList [w])
                ZipList [(.) u] <*> ZipList [v] <*> ZipList [w]    ==    ZipList [u] <*> ZipList [v w]
                                ZipList [u . v] <*> ZipList [w]    ==    ZipList [u (v w)]
                                            ZipList [(u . v) w]    ==    ZipList [u (v w)]    -- satisfied!

With all Applicative Functor laws satisfied, we have proven that this is a
correct implemetation.


2. why ZipList is not suitable as a Monad?

If we want to make a Monad ZipList instance, we'll have to build it on top of
current definition of Applicative ZipList, which means:

    return x = pure x = ZipList (repeat x)

in plain English, each time we lift a normal value x into its monadic context,
it becomes a infinite list of itself. Given this premise, let's try to implement
the (>>=) function to satisfy monad laws.

For the left identity law (return a >>= f == f a), if we could somehow satisfy:

    ZipList (repeat a) >>= f    ==    f a

But wait, the left hand side will be infinite, but we can never guarantee that
the right hand side (f a) will be infinite given that f is unknown.

I made several attempts but couldn't even make it satisfy the first law, left
identity. I found a few discussions (on Haskell-Cafe mailing list, Haskell on
Reddit, and Quora) about this topic, but none of the solutions seen in those
discussions satisfy all three monad laws.

So while the conclusion is clear that ZipList is not suitable as a monad, the
reason, I guess, is that when we build ZipList as Applicative Functor, we have
to make `pure` function to return an infinite list to deal with two lists with
different size, which makes (>>=) impossible to be implemented.
