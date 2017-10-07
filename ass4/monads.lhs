> module Box where


==============================================
              Question 1 Code
==============================================


> import Control.Monad

Type declaration:

> newtype Box a = Box a deriving (Show, Eq)

To be a Monad, Box must be a Functor first.

> instance Functor Box where
>   fmap f (Box x) = Box (f x)

Box also must be an Applicative Functor.

> instance Applicative Box where
>   pure  = Box
>   (<*>) = ap

Now Box is ready to be a Monad.

> instance Monad Box where
>   (Box x) >>= f = f x
> --  return = pure  -- it's the default implementation


==============================================
            Question 1 Discussion
==============================================


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


==============================================
            Question 1 Test Cases
==============================================


test_q1_fmap:
To test that `fmap` works as expected.

> test_q1_fmap :: Bool
> test_q1_fmap = all (== True) [t1, t2, t3]
>   where t1 = fmap (*2) (Box 5) == Box 10
>         t2 = fmap (filter even) (Box [1,2,3,4,5,6]) == Box [2,4,6]
>         t3 = fmap ((*2) . (+5)) (Box 5) == Box 20

test_q1_functorLaws:
To test that Box obeys Functor laws.

> test_q1_functorLaws :: Bool
> test_q1_functorLaws = all (== True) [t1, t2]
>   where t1 = fmap id (Box 5) == id (Box 5)
>         t2 = fmap ((*2) . (+5)) (Box 5) == (fmap (*2) . fmap (+5)) (Box 5)

test_q1_applicativeLaws:
To test that Box obeys Applicative Functor laws.

> test_q1_applicativeLaws :: Bool
> test_q1_applicativeLaws = all (== True) [t1, t2, t3]
>   where t1 = (pure id <*> Box 5) == Box 5
>         t2 = (Box (*2) <*> pure 5) == (pure ($ 5) <*> Box (*2))
>         t3 = (pure (.) <*> Box (*2) <*> Box (+5) <*> Box 5) == (Box (*2) <*> (Box (+5) <*> Box 5))

test_q1_monadLaws:
To test that Box obeys Monad laws (though test1 is using a general monad
instead of Box monad).

> test_q1_monadLaws :: Bool
> test_q1_monadLaws = all (== True) [t1, t2, t3]
>   where t1  = (return 5 >>= \x -> Box (show x)) == Box "5"
>         t2  = (Box 5 >>= return) == Box 5
>         t3  = ((Box 5 >>= f) >>= g) == (Box 5 >>= (\x -> f x >>= g))
>         f x = Box ((+5) x)
>         g x = Box (show x)


theTest_q1:
One test to test them all!

> theTest_q1 :: Bool
> theTest_q1 = all (== True) [
>                test_q1_fmap, test_q1_functorLaws, test_q1_applicativeLaws,
>                test_q1_monadLaws
>              ]


==============================================
              Question 3 Code
==============================================


Type declaration:

> data LockableBox a b = LockedBox a | UnlockedBox b deriving (Show, Eq)

To be a Monad, LockableBox must be a Functor first.

> instance Functor (LockableBox a) where
>   fmap f (UnlockedBox x) = UnlockedBox (f x)
>   fmap _ (LockedBox x)   = LockedBox x

LockableBox also must be an Applicative Functor.

> instance Applicative (LockableBox a) where
>   pure = UnlockedBox

>   (UnlockedBox f) <*> (UnlockedBox x) = UnlockedBox (f x)
>   (UnlockedBox _) <*> (LockedBox x)   = LockedBox x
>   (LockedBox f)   <*> (UnlockedBox _) = LockedBox f
>   (LockedBox f)   <*> (LockedBox _)   = LockedBox f

Now LockableBox is ready to be a Monad.

> instance Monad (LockableBox a) where
>   (UnlockedBox x) >>= f = f x
>   (LockedBox x)   >>= _ = LockedBox x

lock:
Lock an unlocked box. Will raise error if the box passed in is already locked.

> lock :: LockableBox a b -> LockableBox b b
> lock (UnlockedBox x) = LockedBox x
> lock (LockedBox _)   = error "Illegal argument: Cannot lock a locked box."

unlock:
Unlock a locked box. Will raise error if the box passed in is already unlocked.

> unlock :: LockableBox a b -> LockableBox a a
> unlock (LockedBox x)   = UnlockedBox x
> unlock (UnlockedBox _) = error "Illegal argument: Cannot unlock an unlocked box."


==============================================
            Question 3 Discussion
==============================================

1. Why did you decide on the implementations you did?






2. The lock and unlock functions have some difficulties. Why? Could a lockable
box that wasn't a monad be different?






==============================================
            Question 3 Test Cases
==============================================


Note:

We can't compare function equality in Haskell, so there are many laws I
can't test, for example, `LockedBox (*2) <*> UnlockedBox 5 == LockedBox (*2)`
can't be written as a test case.

There are many test cases that are commented out in the following tests. They
serve as a proof of concept that laws to be tested still hold even we can't
directly test it.


test_q3_fmap:
To test that `fmap` works as expected. Covers both UnlockedBox & LockedBox

> test_q3_fmap :: Bool
> test_q3_fmap = all (== True) [t1, t2, t3, t4, t5, t6]
>   where t1 = fmap (*2) (UnlockedBox 5) == (UnlockedBox 10 :: LockableBox Int Int)
>         t2 = fmap (*2) (LockedBox 5) == LockedBox 5
>         t3 = fmap (filter even) (UnlockedBox [1,2,3,4,5,6]) == (UnlockedBox [2,4,6] :: LockableBox Int [Int])
>         t4 = fmap (filter even) (LockedBox [1,2,3,4,5,6]) == LockedBox [1,2,3,4,5,6]
>         t5 = fmap ((*2) . (+5)) (UnlockedBox 5) == (UnlockedBox 20 :: LockableBox Int Int)
>         t6 = fmap ((*2) . (+5)) (LockedBox 5) == LockedBox 5

test_q3_star:
To test that `(<*>)` works as expected

> test_q3_star :: Bool
> test_q3_star = all (== True) [t1, t2, t5, t6]
>   where t1 = (UnlockedBox (*2) <*> UnlockedBox 5) == (UnlockedBox 10 :: LockableBox Int Int)
>         t2 = (UnlockedBox (*2) <*> LockedBox 5)   == LockedBox 5
>      -- t3 = (LockedBox (*2)   <*> UnlockedBox 5) == LockedBox (*2)
>      -- t4 = (LockedBox (*2)   <*> LockedBox 5)   == LockedBox (*2)
>         t5 = ((*) <$> UnlockedBox 2 <*> UnlockedBox 5) == (UnlockedBox 10 :: LockableBox Int Int)
>         t6 = ((*) <$> LockedBox 2 <*> LockedBox 5) == LockedBox 2

test_q3_bind:
To test that `(>>=)` works as expected

> test_q3_bind :: Bool
> test_q3_bind = all (== True) [t1, t2]
>   where t1 = (unlockedBox >>= \x -> UnlockedBox (show x)) == UnlockedBox "5"
>         t2 = (lockedBox >>= \x -> UnlockedBox (show x)) == LockedBox 5

test_q3_functorLaws:
To test that LockableBox obeys Functor laws.

> test_q3_functorLaws :: Bool
> test_q3_functorLaws = all (== True) [t1, t2, t3, t4]
>   where t1 = fmap id unlockedBox == id unlockedBox
>         t2 = fmap id lockedBox == id lockedBox
>         t3 = fmap ((*2) . (+5)) unlockedBox == (fmap (*2) . fmap (+5)) unlockedBox
>         t4 = fmap ((*2) . (+5)) lockedBox == (fmap (*2) . fmap (+5)) lockedBox
>         unlockedBox = UnlockedBox 5 :: LockableBox Int Int
>         lockedBox   = LockedBox 5 :: LockableBox Int Int

test_q3_applicativeLaws:
To test that LockableBox obeys all Applicative Functor laws.

> test_q3_applicativeLaws :: Bool
> test_q3_applicativeLaws = all (== True) [
>                             t01, t02,  -- t03 & t04: identity
>                             t03,       -- t03 & t04: interchange
>                             t05, t06   -- t05 - t13: composition
>                           ]
>   where t01 = (pure id <*> unlockedBox) == unlockedBox
>         t02 = (pure id <*> lockedBox) == lockedBox
>         t03 = ((UnlockedBox (*2) <*> pure 5) :: LockableBox Int Int) == ((pure ($ 5) <*> UnlockedBox (*2)) :: LockableBox Int Int)
>      -- t04 = (LockedBox (*2) <*> pure 5) == (pure ($ 5) <*> LockedBox (*2))
>      --   t05 = ((pure (.) <*> UnlockedBox (*2) <*> UnlockedBox (+5) <*> UnlockedBox 5) :: LockableBox Int Int)
>      --           == ((UnlockedBox (*2) <*> (UnlockedBox (+5) <*> UnlockedBox 5)) :: LockableBox Int Int)
>         t05 = (pure (.) <*> UnlockedBox (*2) <*> UnlockedBox (+5) <*> unlockedBox)   == (UnlockedBox (*2) <*> (UnlockedBox (+5) <*> unlockedBox))
>         t06 = (pure (.) <*> UnlockedBox (*2) <*> UnlockedBox (+5) <*> LockedBox 5)   == (UnlockedBox (*2) <*> (UnlockedBox (+5) <*> LockedBox 5))
>      -- t07 = (pure (.) <*> UnlockedBox (*2) <*> LockedBox (+5)   <*> UnlockedBox 5) == (UnlockedBox (*2) <*> (LockedBox (+5)   <*> UnlockedBox 5))
>      -- t08 = (pure (.) <*> LockedBox (*2)   <*> UnlockedBox (+5) <*> UnlockedBox 5) == (LockedBox (*2)   <*> (UnlockedBox (+5) <*> UnlockedBox 5))
>      -- t09 = (pure (.) <*> UnlockedBox (*2) <*> LockedBox (+5)   <*> LockedBox 5)   == (UnlockedBox (*2) <*> (LockedBox (+5)   <*> LockedBox 5))
>      -- t10 = (pure (.) <*> LockedBox (*2)   <*> LockedBox (+5)   <*> UnlockedBox 5) == (LockedBox (*2)   <*> (LockedBox (+5)   <*> UnlockedBox 5))
>      -- t11 = (pure (.) <*> LockedBox (*2)   <*> UnlockedBox (+5) <*> LockedBox 5)   == (LockedBox (*2)   <*> (UnlockedBox (+5) <*> LockedBox 5))
>      -- t12 = (pure (.) <*> LockedBox (*2)   <*> LockedBox (+5)   <*> LockedBox 5)   == (LockedBox (*2)   <*> (LockedBox (+5)   <*> LockedBox 5))

test_q3_monadLaws:
To test that LockableBox obeys Monad laws (though test1 is using a general monad
instead of LockableBox monad).

> test_q3_monadLaws :: Bool
> test_q3_monadLaws = all (== True) [t1, t2, t3, t4, t5, t6]
>   where t1 = (unlockedBox >>= \x -> UnlockedBox (show x)) == UnlockedBox "5"
>         t2 = (lockedBox >>= \x -> UnlockedBox (show x)) == LockedBox 5
>         t3 = (unlockedBox >>= return) == unlockedBox
>         t4 = (lockedBox >>= return) == lockedBox
>         t5  = ((unlockedBox >>= f) >>= g) == (unlockedBox >>= (\x -> f x >>= g))
>         t6  = ((lockedBox >>= f) >>= g) == (lockedBox >>= (\x -> f x >>= g))
>         f x = UnlockedBox ((+5) x)
>         g x = UnlockedBox (show x)

unlockedBox:
a mock subject of UnlockedBox.

> unlockedBox :: LockableBox Int Int
> unlockedBox = UnlockedBox 5

lockedBox:
a mock subject of LockedBox.

> lockedBox :: LockableBox Int Int
> lockedBox = LockedBox 5


theTest_q3:
One test to test them all!

> theTest_q3 :: Bool
> theTest_q3 = all (== True) [
>                test_q3_fmap, test_q3_star, test_q3_bind, test_q3_functorLaws,
>                test_q3_applicativeLaws, test_q3_monadLaws
>              ]
