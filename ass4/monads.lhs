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
>   where t1  = (return 5 >>= \x -> show x) == show 5
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
              Question 2 Code
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
>   _               <*> (LockedBox x)   = LockedBox x
>   (LockedBox f)   <*> (UnlockedBox _) = LockedBox f


pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition




Now Box is ready to be a Monad.

> instance Monad (LockableBox a) where
>   (UnlockedBox x) >>= f = f x
>   (LockedBox x) >>= _ = LockedBox x
>   return = pure


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












==============================================
            Question 3 Test Cases
==============================================







One test to test them all!

> theTest_q3 :: Bool
> theTest_q3 = all (== True) [
>
>              ]
