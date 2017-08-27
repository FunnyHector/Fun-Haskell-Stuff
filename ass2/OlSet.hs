-- Represented using ordered list
module OlSet (
  Set,   -- no data constructor exported. Have to make a Set using function `makeSet`.
  makeSet,
  has,
  card,
  add,
  del,
  union,
  intersect,
  equals,
  subset,
  select,

-- Additional functions:
  emptySet,
  orderedList,
  difference,
  toList,
  isEmpty,
  mapSet,
  partition,
  foldSet
) where

{---------------------------------------------
               Type declaration
---------------------------------------------}

newtype Set a = Set { orderedList :: [a] } deriving (Show)

instance Ord a => Eq (Set a) where
  (==) = equals

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

-- | Form the union of two Sets, i.e. the set of elements that occur in either
--   (or both) of the sets
union :: Ord a => Set a -> Set a -> Set a
union (Set []) set      = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
union set (Set [])      = set
union (Set xs) (Set ys) = Set (orderedUniqueList $ xs ++ ys)

-- | Form the intersection of two sets, i.e. the set of all elements that occur
--   in both sets.
intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set []) _        = emptySet  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
intersect _ (Set [])        = emptySet
intersect (Set xs) (Set ys) = Set (orderedUniqueList $ intersectList xs ys)

-- | Determine whether two sets are equal, i.e. whether every element that occurs
--   in either of the sets also occurs in the other.
equals :: Ord a => Set a -> Set a -> Bool
equals setA setB = orderedList setA == orderedList setB

-- | Determine whether one set is contained in another, i.e. whether every element
--   that occurs in the first set also occurs in the second.
subset :: Ord a => Set a -> Set a -> Bool
subset (Set xs) (Set ys) = all (`elem` ys) xs

-- | Return the set of elements of a given set satisfying a given property
select :: Ord a => (a -> Bool) -> Set a -> Set a
select f (Set xs) = Set (filter f xs)

{---------------------------------------------
            Additional functions
        (not required in the handout)
---------------------------------------------}

-- | Return an empty set
emptySet :: Set a
emptySet = Set []

-- | Form the difference of two sets, i.e. the set of elements that occur only in
--   the first set but not in the second set.
difference :: Ord a => Set a -> Set a -> Set a
difference set (Set [])      = set
difference (Set []) _        = emptySet
difference (Set xs) (Set ys) = Set (filter (`notElem` ys) xs)

-- | Convert the set to an ordered list.
toList :: Set a -> [a]
toList = orderedList

-- | Determine whether the set is empty
isEmpty :: Set a -> Bool
isEmpty set = card set == 0

-- | Return a Set obtained by applying function f to each element of the given
--   Set. Note that the size of returned set is less or equal to the size of
--   original set.
mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = (makeSet . orderedUniqueList . map f) xs

-- | Partition the set using the given predicate, and return two sets: the first
--   one has all elements satisfying the predicate, and the second one has the
--   rest.
partition :: (a -> Bool) -> Set a -> (Set a,Set a)
partition f (Set xs) = (Set $ filter f xs, Set $ filter (not . f) xs)

-- | Fold the elements in the set using the given binary operator.
--   N.B. internally this is applying `foldr` on the ordered list, so the given
--   function should be (\element, identity -> element `f` identity)
foldSet :: (Ord a, Ord b) => (a -> b -> b) -> b -> Set a -> b
foldSet f identity (Set xs) = foldr f identity xs

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

-- | Test whether a list contains a given element using binary search.
--   N.B. the given list has to be sorted, otherwise the result is chaotic.
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs
  | x == xs !! mid = True
  | x <  xs !! mid = binarySearch x left
  | otherwise      = binarySearch x right
  where mid           = length xs `div` 2
        (left, right) = (take mid xs, drop (mid + 1) xs)

-- | Intersect two lists
intersectList :: Ord a => [a] -> [a] -> [a]
intersectList xs ys = foldr (\e result -> if e `elem` xs && e `elem` ys then e:result else result) [] ys
