module Bag where

type Map a b = [(a, b)]

newtype Bag a = Bag (Map a Int) deriving Show

--------------- Map functions -------------------

hasKey :: Eq a => a -> Map a b -> Bool
hasKey _ [] = False
hasKey x ((k, _) : pairs)
  | x == k    = True
  | otherwise = hasKey x pairs

delKey :: Eq a => a -> Map a b -> Map a b
delKey _ [] = []
delKey x ((k, _) : pairs)
  | k == x    = pairs
  | otherwise = delKey x pairs

getVal :: Eq a => a -> Map a b -> Maybe b
getVal _ [] = Nothing
getVal x ((k, v) : pairs)
  | x == k    = Just v
  | otherwise = getVal x pairs

setVal :: Eq a => a -> b -> Map a b -> Map a b
setVal x y [] = [(x, y)]
setVal x y ((k, _) : pairs)
  | x == k    = (k, y) : pairs
  | otherwise = setVal x y pairs

--------------- Bag functions -----------------

add :: Eq a => a -> Bag a -> Bag a
add = addN 1

addN :: Eq a => Int -> a -> Bag a -> Bag a
addN n x (Bag m)
  | hasKey x m = Bag (setVal x (v + n) m)
  | otherwise  = Bag (setVal x n m)
  where Just v = getVal x m

del :: Eq a => a -> Bag a -> Bag a
del = delN 1

delN :: Eq a => Int -> a -> Bag a -> Bag a
delN n x bag@(Bag m)
  | not (hasKey x m)     = bag
  | hasKey x m && v <= n = Bag (delKey x m)
  | otherwise            = Bag (setVal x (v - n) m)
  where Just v = getVal x m

union :: Eq a => Bag a -> Bag a -> Bag a
union bag (Bag []) = bag
union bag (Bag ((k, v) : pairs)) = union (addN v k bag) (Bag pairs)

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag (Bag []) _ = True
subbag (Bag ((k, v1) : pairs)) bag@(Bag m)
  | hasKey k m && v1 <= v2 = subbag (Bag pairs) bag
  | otherwise              = False
  where Just v2 = getVal k m
