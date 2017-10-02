module Test where

import Control.Monad.Reader

dataset :: [Float]
dataset = [5, 8, 13, 22, 6, 9]

average :: (Fractional a) => [a] -> a
average = do s <- sum
             b <- realToFrac . length
             return (s / b)

average2 :: (Fractional a) => [a] -> a
average2 = sum >>= \s -> realToFrac . length
               >>= \b -> return (s / b)


stddev :: (Floating a) => [a] -> a
stddev  = do s <- sum
             b <- realToFrac . length
             d <- map ((^2) . ((s / b)-))
             return (sqrt (sum d / b))

-- local :: (r -> r) -> m a -> m a
useless :: [Int] -> Int
useless = do x <- local (const [4,5,6]) sum
             return x
-- ask :: m r
ident :: a -> a
ident = do x <- ask
           return x
-- reader :: (r -> a) -> m a
first :: [a] -> a
first = do x <- reader head
           return x





tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this?"
