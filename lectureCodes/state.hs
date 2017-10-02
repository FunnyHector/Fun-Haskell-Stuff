import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map

simple :: State Int Int
simple = do put 7
            x <- return 5
            y <- get
            return (x * y)

-- Counts how many times tick has been called
-- Returns the old count
tick :: State Int Int
tick = do
  n <- get
  put (n + 1)
  return n


tickDemo = flip runState 0 $ do tick
                                tick
                                x <- tick
                                tick
                                return (10 + x)




-- Returns the length of the string in the state
-- concatenated with the string given as argument.
myfunc :: String -> State String Int
myfunc v = state $ \s -> (length (v ++ s), v)

myDemo = flip runState "hello" $
           do x <- myfunc "test"
              return x







-- ---------------------- --
-- Stack-based calculator --
-- ---------------------- --

push :: Int -> State [Int] ()
push i = state $ \xs -> ((), i:xs)

pop :: State [Int] Int
pop = state $ \(x:xs) -> (x, xs)

-- Pure stack computation
-- Run as runState pushPop []
pushPop = do push 7    -- Stack: [7]
             push 3    -- Stack: [3,7]
             push 5    -- Stack: [5,3,7]
             pop       -- Stack: [3,7]
             x <- pop  -- Stack: [7]
             push 9    -- Stack: [9,7]
             return x


add :: State [Int] Int
add = do x <- pop
         y <- pop
         push (x + y)
         return (x + y)

mul :: State [Int] Int
mul = state $ \(x:y:xs) -> (x * y, (x * y):xs)









-- Simulate named mutable variables
set :: String -> a -> State (Map String a) ()
set s v = state $ \m -> ((), insert s v m)

var :: String -> State (Map String a) a
var s = state $ \m -> let (Just v) = lookup s m in (v, m)

mutTest :: State (Map String Int) Int
mutTest = do set "x" 5
             set "y" 7
             val <- var "x"
             set "x" (val + 1)
             val <- var "y"
             val2 <- var "x"
             return (val * val2)

