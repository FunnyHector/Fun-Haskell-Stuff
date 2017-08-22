module SillySort (
  sort,
  perms,
  asc
) where

-- | An interesting approach of sort. What it does is iterating through all
--   permutations until it finds a list that is in non-descending order.
sort :: Ord a => [a] -> [a]
sort l = head [s | s <- perms l, asc s]

-- | Returns a list of permutations of a given list
perms :: Ord a => [a] -> [[a]]
perms [] = [[]]
perms xs = [y:ys | y <- xs, ys <- perms (delelteFirst y xs)]

-- | Determins whether a list is in non-descending order
asc :: Ord a => [a] -> Bool
asc []  = True
asc [_] = True
asc (x:l@(y:_))
  | x > y     = False
  | otherwise = asc l

{---------------------------------------------
               Internal functions
---------------------------------------------}

-- | Delete the first occurrence of x from a list
delelteFirst :: Eq a => a -> [a] -> [a]
delelteFirst _ [] = []
delelteFirst x (y:ys)
  | x == y = ys
  | otherwise = y : delelteFirst x ys
