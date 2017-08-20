-- Represented using ordered list
module Set (
  Set,   -- no data constructor exported. Have to make a Set using function `makeSet`.
  makeSet,
  has,
  card,
  add,
  del,
--   union,
--   intersect,
--   equals,
--   subset,
--   select,
--
-- -- Additional functions:
--   difference,
--   toList,
--   isEmpty,
--   mapSet,
--   partition,
--   foldSet
  )
where

{---------------------------------------------
               Type declaration
---------------------------------------------}

newtype Set a = Set { orderedList :: [a] } deriving (Show)

-- instance Eq a => Eq (Set a) where
--   (==) = equals
--   (/=) = notEquals

{---------------------------------------------
                API Functions
---------------------------------------------}

-- | Create a Set with a given list of values, ignoring repeated elements.
makeSet :: Ord a => [a] -> Set a
makeSet xs = Set $ orderedUniqueList xs

-- | Test whether a Set contains a given element. This function is using binary
--   search.
has :: Ord a => a -> Set a -> Bool
has x (Set xs) = binarySearch x xs

-- | Find the cardinality (number of elements) of a Set
card :: Set a -> Int
card (Set xs) = length xs

-- | Add an element to a Set, leaving the Set unchanged if it is already there
add :: Ord a => a -> Set a -> Set a
add x (Set xs)
  | binarySearch x xs = Set xs
  | otherwise         = Set $ quicksort (x:xs)

-- | Delete an element from a Set, leaving the Set unchanged if it is not there
del :: Ord a => a -> Set a -> Set a
del x (Set xs)
  | binarySearch x xs = Set (filter (/=x) xs)
  | otherwise         = Set xs


{---------------------------------------------
               Internal functions
---------------------------------------------}

-- | Make a sorted list that only contains unique elements from a given list.
--   Same as Data.List.sort . Data.List.nub
orderedUniqueList :: Ord a => [a] -> [a]
orderedUniqueList = quicksort . foldr (\e result -> if e `elem` result then result else e:result) []

-- | Quick sort. From https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
  where (lesser, greater) = (filter (< p) xs, filter (>= p) xs)

-- | Test whether a Set contains a given element using binary search.
--   N.B. the given list has to be sorted, otherwise the result is chaotic.
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs
  | x == xs !! mid = True
  | x <  xs !! mid = binarySearch x left
  | otherwise      = binarySearch x right
  where mid           = length xs `div` 2
        (left, right) = (take mid xs, drop (mid + 1) xs)
