Name:              Fang Zhao (300364061)
Course Number:     COMP304
Assignment Number: 4
Question Number:   1, 3, & 4

> module Box where


==============================================
              Question 1 Code
==============================================


> import Control.Monad
> import Control.Monad.State

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
To test that Box obeys Monad laws.

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
>   pure                = UnlockedBox
>   LockedBox   f <*> _ = LockedBox f
>   UnlockedBox f <*> v = fmap f v

Now LockableBox is ready to be a Monad.

> instance Monad (LockableBox a) where
>   LockedBox   x >>= _ = LockedBox x
>   UnlockedBox x >>= f = f x

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

- LockableBox as Monad is like Either Monad, if we treat LockedBox as the Left
case. What's more than Left of Either is, the "failure" (LockedBox) can be
recovered/brought back to UnlockedBox.

So my implementation looks like how Either Monad is implemented a lot. Generally
speaking, as can be seen from code part above, we can simply apply `fmap`,
`(<*>)`, `(>>=)` on UnlockedBox like normal box from question 1; but if we try
to do anything to a LockedBox, all we get out at the end is that LockedBox.

Test cases are provided to validate that my implementation satisfies Monad laws,
Applicative Functor laws, and Functor laws.

There is one arguable implementation: should `(LockedBox f) <*> (LockedBox x)`
return `LockedBox f` or `LockedBox x`? In my opinion, both approaches satisfy
all the laws. I chose `(LockedBox f) <*> (LockedBox _) = LockedBox f`.


2. The lock and unlock functions have some difficulties. Why? Could a lockable
box that wasn't a monad be different?

- The `lock` and `unlock` function have some difficulties, because Haskell has
very strict type mechanism. I believe this has nothing to do with being a Monad
or being a Functor. LockableBox being a Functor is the root cause, because in
order to properly apply `fmap` on LockableBox, we have to use two type variables
`a` and `b` for LockedBox and UnlockedBox.

When we do `lock (UnlockedBox x)`, the type of `UnlockedBox x` is
`LockableBox a b`, where `b` is the type of `x`. At this point there is a hidden
type `a`, which we/Haskell compiler can never know what actual type it is. But
we don't care about type `a`, because what we want to return is `LockedBox x`,
which has type `LockableBox b b`. This is pretty straightforward. The tricky
part comes when we do `lock (LockedBox x)`. The type of `LockedBox x` is
`LockableBox a b`, where `a` is the type of `x`. This time `b` is the hidden
type which we/compiler can never figure out, and we need to return something
with type `LockableBox b b`. Dead end. The easiest way out here is to simply
raise an error to disallow locking a LockedBox, which is not that ideal, but
works fine with Haskell type mechanism.

What happens when we `unlock` a LockableBox is pretty much the same with when we
`lock` it.

Now consider an alternative version of LockableBox, which is not a Functor:

  data LockableBox a = LockedBox a | UnlockedBox a deriving (Show, Eq)

  lock :: LockableBox a -> LockableBox a
  lock (LockedBox x)   = LockedBox x
  lock (UnlockedBox x) = LockedBox x

  unlock :: LockableBox a -> LockableBox a
  unlock (LockedBox x)   = UnlockedBox x
  unlock (UnlockedBox x) = UnlockedBox x

Implementing `lock` and `unlock` is much easier and more intuitive. However, we
lose all convenience of Functor/Applicative/Monad.


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
>         t05 = (pure (.) <*> UnlockedBox (*2) <*> UnlockedBox (+5) <*> unlockedBox)   == (UnlockedBox (*2) <*> (UnlockedBox (+5) <*> unlockedBox))
>         t06 = (pure (.) <*> UnlockedBox (*2) <*> UnlockedBox (+5) <*> LockedBox 5)   == (UnlockedBox (*2) <*> (UnlockedBox (+5) <*> LockedBox 5))
>      -- t07 = (pure (.) <*> UnlockedBox (*2) <*> LockedBox (+5)   <*> UnlockedBox 5) == (UnlockedBox (*2) <*> (LockedBox (+5)   <*> UnlockedBox 5))
>      -- t08 = (pure (.) <*> LockedBox (*2)   <*> UnlockedBox (+5) <*> UnlockedBox 5) == (LockedBox (*2)   <*> (UnlockedBox (+5) <*> UnlockedBox 5))
>      -- t09 = (pure (.) <*> UnlockedBox (*2) <*> LockedBox (+5)   <*> LockedBox 5)   == (UnlockedBox (*2) <*> (LockedBox (+5)   <*> LockedBox 5))
>      -- t10 = (pure (.) <*> LockedBox (*2)   <*> LockedBox (+5)   <*> UnlockedBox 5) == (LockedBox (*2)   <*> (LockedBox (+5)   <*> UnlockedBox 5))
>      -- t11 = (pure (.) <*> LockedBox (*2)   <*> UnlockedBox (+5) <*> LockedBox 5)   == (LockedBox (*2)   <*> (UnlockedBox (+5) <*> LockedBox 5))
>      -- t12 = (pure (.) <*> LockedBox (*2)   <*> LockedBox (+5)   <*> LockedBox 5)   == (LockedBox (*2)   <*> (LockedBox (+5)   <*> LockedBox 5))

test_q3_monadLaws:
To test that LockableBox obeys Monad laws.

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


==============================================
              Question 4 Code
==============================================


A type synonym. READABILITY BABE!

> type Stack = [Int]

pop:
Pop out the element on top of the stack.

> pop :: State Stack Int
> pop = state $ \(x : xs) -> (x, xs)

push:
Push a new element on top of the stack

> push :: Int -> State Stack ()
> push a = state $ \xs -> ((), a : xs)

dup:
Duplicate the element on top of the stack, and return `()` with the new state.
If the stack is empty, then return `()` and the same state (do nothing).

> dup :: State Stack ()
> dup = state $ \s ->
>   case () of
>     _ | not $ null s -> let (x : _) = s in ((), x : s)
>       | otherwise    -> ((), s)

swap:
Swap top two elements of the stack, and return `()` with the new state. If the
stack contains only one or zero element, then return `()` and the same state
(do nothing).

> swap :: State Stack ()
> swap = state $ \s ->
>   case () of
>     _ | length s > 1 -> let (x : y : rest) = s in ((), y : x : rest)
>       | otherwise    -> ((), s)


==============================================
            Question 4 Discussion
==============================================


1. Should it explode or do nothing when `dup`/`swap` is not applicable?

- `dup` is not applicable when the size of the stack is 0, and `swap` is not
applicable when the size is 0 or 1. I'm not a fan of explosions. I think doing
nothing when it's not applicable makes sense here.

2. A style of programming based on implicit stack manipulation like this is
often called concatenative, because two programs can be concatenated to have the
result of one used as the input to the next. Discuss how your system does or
does not behave in this way.

- This Stack system does behave as concatenative programming. Four existing
functions, `push`, `pop`, `dup`, and `swap`, all return a State Monad (in this
case "State Stack"), and Monad is very good at chaining with each other /
concatenatively programming. If we look closely to the following example:

    flip runState [] $ do
      push 3
      push 5
      push 7
      swap
      pop
      dup

and transform the do notation to monadic binding style:

      push 3 >>
      push 5 >>
      push 7 >>
      swap >>
      pop >>= \_ ->
      dup

We can see that they chain perfectly. The function bind `(>>=)` (and the
ignoring bind `(>>)` as well) allows function to use the Monadic value returned
from previous function, so the function flow is like:

    Monad m =>
    m a -> \a -> m b >>= \b -> m c >>= \c -> m d >>= ...

The chain can go as long as we need, and we can easily swap functions because
they all return the same type of Monad State.

The do notation enables us to care more about what functions do, and (sort of)
screen out the monadic context, so we can write imperative-looking programmes as
the example demonstrates.


==============================================
            Question 4 Test Cases
==============================================


test_dup:
Simply test the behaviour of dup. Covers both paths of the function.

> test_dup :: Bool
> test_dup = all (== True) [t1, t2, t3, t4]
>   where t1 = runState dup [] == ((),[])
>         t2 = runState dup [1] == ((),[1, 1])
>         t3 = runState (do push 1; push 2; dup; push 3) [] == ((),[3, 2, 2, 1])
>         t4 = runState (do push 1; pop; dup; push 3) [] == ((),[3])

test_swap:
Simply test the behaviour of swap. Covers both paths of the function.

> test_swap :: Bool
> test_swap = all (== True) [t1, t2, t3, t4]
>   where t1 = runState swap [] == ((),[])
>         t2 = runState swap [1] == ((),[1])
>         t3 = runState (do push 1; push 2; swap; push 3) [] == ((),[3, 1, 2])
>         t4 = runState (do push 1; swap; push 2; swap; push 3; swap) [] == ((),[1, 3, 2])

theTest_q4:
One test to test them all!

> theTest_q4 :: Bool
> theTest_q4 = all (== True) [test_dup, test_swap]
