module Test where

import Prelude hiding (log)
import Control.Monad

-- Writer with log type w and value type a
newtype Writer w a = Writer {runWriter :: (a, [w])} deriving (Show)
instance Monad (Writer w) where
  return x = Writer (x, [])
  -- Join the two logs together, and use the value
  -- that came from calling the function:
  (Writer (x, l1)) >>= f =
    let (Writer (y, l2)) = f x in
        Writer (y, l2 ++ l1)
-- The log doesn't have to be a list, but we've
-- made it so for simplicity's sake for now.

--demo2 :: Writer String ()
demo2 = add 5 3 >>= \_ -> add 6 10

-- Just log a message, with no value.
log :: String -> Writer String ()
log s = Writer((), [s])

-- Add two numbers, and log that it happened.
add :: (Num a, Show a) => a -> a -> Writer String a
add x y = Writer(x + y,
    [(show x) ++ "+" ++ (show y)
      ++ "=" ++ (show (x + y))
    ]
  )

mul :: (Num a, Show a) => a -> a -> Writer String a
mul x y = Writer(x * y,
  [(show x)++"*"++(show y)++"="++(show(x * y))])




demo = runWriter $ do log "Starting"
                      x <- add 5 7
                      y <- mul 3 8
                      if x > y
                         then add x y
                         else mul x y
                      z <- add x y
                      return z











-- Generic monad-as-functor instances:
instance Functor (Writer w) where
    fmap = liftM
instance Applicative (Writer w) where
    pure = return
    (<*>) = ap
