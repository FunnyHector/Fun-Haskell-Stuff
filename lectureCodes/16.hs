module Test where

import Control.Monad
import Data.List

lcomp = [ x * y | x <- [1..10], x > 5, y <- [2..10], mod x y == 0 ]

ldo = do x <- [1..4]
         y <- [2..5]
         [x,y,100]  -- return x => (: []) x => x : [] => [x]
                    -- so here [x,y,100] is same as return x,y,100, three of them together, although syntactically illegal

ldo' = do x <- [1..4]
          y <- [2..5]
          return [x,y,100]  -- note the difference with ldo

xs >>= f = concat (map f xs)


lapp = [(+2), (*10), (50-)] <*> [10,20,30]

lapp2 = [(+), (*)] <*> [10,20,30] <*> [100,200]

lapp2' = do f <- [(+), (*)]
            x <- [10,20,30]
            y <- [100, 200]
            return (f x y)














mess :: [Int] -> [[Int]]
mess l = do x <- l
            y <- l
            z <- permutations l
            return ((x+y) : z)

-- n queens problem: place n queens on a chess board in positions
-- where none of them can capture any other.
queens n = queensN n []

-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- Composes two monadic functions together

queensN n start = do x <- return start
                     foldr (<=<) return (replicate n (queens' n)) x

queens' :: Int -> [Int] -> [[Int]]
queens' n cur = do x <- [1..n]
                   guard $ not (x `elem` cur)
                   guard $ valid (x : cur)
                   return (x : cur)

-- Determine whether a list of queen coordinates is valid (no captures).
valid :: [Int] -> Bool
valid [] = True
valid [_] = True
valid (x:xs) = (valid' xs (x-1) (x+1)) && valid xs

-- Helper for valid. Proceeds rightwards along the diagonals upwards and
-- downwards, checking whether a queen is in that position.
valid' :: [Int] -> Int -> Int -> Bool
valid' [] _ _ = True
valid' (x:xs) up down -- Use guards to simplify this test.
    | x == up = False -- These two match when x is on the diagonal
    | x == down = False -- vv Recurse, moving one along the diagonal in
    | otherwise = valid' xs (up-1) (down+1) -- each direction. xs is
                                            -- strictly smaller, so this
                                            -- terminates.


-- Convert board to string for show
showQueens :: [Int] -> String
showQueens qs = foldl1 (++) [showQueens' q (foldl1 max qs)|q<-qs]

showQueens' :: Int-> Int-> String
showQueens' q l = [' '|x<-[1..q-1]] ++ 'Q' : [' '|x<-[1..l-q] ] ++ "\n"
