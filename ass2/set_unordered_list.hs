-- Represented using unordered list
module Set (
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

newtype Set a = Set { unorderedList :: [a] } deriving (Show)

instance Eq a => Eq (Set a) where
  (==) = equals
  (/=) = notEquals

{---------------------------------------------
                API Functions
---------------------------------------------}

-- | Create a Set with a given list of values, ignoring repeated elements.
makeSet :: Eq a => [a] -> Set a
makeSet xs = Set $ uniqueList xs

-- | Test whether a Set contains a given element
has :: Eq a => a -> Set a -> Bool
has x (Set xs) = x `elem` xs

-- | Find the cardinality (number of elements) of a Set
card :: Set a -> Int
card (Set xs) = length xs

-- | Add an element to a Set, leaving the Set unchanged if it is already there
add :: Eq a => a -> Set a -> Set a
add x (Set xs)
  | x `elem` xs = Set xs
  | otherwise   = Set (x:xs)   -- add the element at the head. Adding in front is faster than at the end, because `:` is more efficient than `++`

-- | Delete an element from a Set, leaving the Set unchanged if it is not there
del :: Eq a => a -> Set a -> Set a
del x (Set xs)
  | x `elem` xs = Set (filter (/=x) xs)
  | otherwise   = Set xs

-- | Form the union of two Sets, i.e. the set of elements that occur in either
--   (or both) of the sets
union :: Eq a => Set a -> Set a -> Set a
union (Set []) set      = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
union set (Set [])      = set
union (Set xs) (Set ys) = Set (uniqueList $ xs ++ ys)

-- | Form the intersection of two sets, i.e. the set of all elements that occur
--   in both sets.
intersect :: Eq a => Set a -> Set a -> Set a
intersect (Set []) _        = emptySet  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
intersect _ (Set [])        = emptySet
intersect (Set xs) (Set ys) = Set (uniqueList $ intersectList xs ys)

-- | Determine whether two sets are equal, i.e. whether every element that occurs
--   in either of the sets also occurs in the other.
equals :: Eq a => Set a -> Set a -> Bool
equals setA setB = subset setA setB && subset setB setA

-- | Determine whether one set is contained in another, i.e. whether every element
--   that occurs in the first set also occurs in the second.
subset :: Eq a => Set a -> Set a -> Bool
subset (Set xs) (Set ys) = all (`elem` ys) xs

-- | Return the set of elements of a given set satisfying a given property
select :: Eq a => (a -> Bool) -> Set a -> Set a
select f (Set xs) = Set (filter f xs)

{---------------------------------------------
            Additional functions
        (not required in the handout)
---------------------------------------------}

-- | Form the difference of two sets, i.e. the set of elements that occur only in
--   the first set but not in the second set.
difference :: Eq a => Set a -> Set a -> Set a
difference set (Set [])          = set
difference (Set []) _            = emptySet
difference set@(Set xs) (Set ys) = select (\x -> x `elem` xs && x `notElem` ys) set

-- | Convert the set to a list. No order guaranteed.
toList :: Set a -> [a]
toList = unorderedList

-- | Determine whether the set is empty
isEmpty :: Set a -> Bool
isEmpty set = card set == 0

-- | Return a Set obtained by applying function f to each element of the given
--   Set. Note that the size of returned set is less or equal to the size of
--   original set.
mapSet :: (Eq a, Eq b) => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = (makeSet . uniqueList . map f) xs

-- | Partition the set using the given predicate, and return two sets: the first
--   one has all elements satisfying the predicate, and the second one has the
--   rest.
partition :: (a -> Bool) -> Set a -> (Set a,Set a)
partition f (Set xs) = (Set $ filter f xs, Set $ filter (not . f) xs)

-- | Fold the elements in the set using the given binary operator
--   N.B. internally this is applying `foldr` on the unordered list, so the given
--   function should be (\element, identity -> element `f` identity)
foldSet :: (Eq a, Eq b) => (a -> b -> b) -> b -> Set a -> b
foldSet f identity (Set xs) = foldr f identity xs

{---------------------------------------------
               Internal functions
---------------------------------------------}

-- | Make a list that only contains unique elements from a given list. Same as Data.List.nub.
uniqueList :: Eq a => [a] -> [a]
uniqueList = foldr (\e result -> if e `elem` result then result else e:result) []

-- | Intersect two lists.
intersectList :: Eq a => [a] -> [a] -> [a]
intersectList xs ys = foldr (\e result -> if e `elem` xs && e `elem` ys then e:result else result) [] ys

-- | Determine whether two sets are not equal.
--   I think this approach would be faster than `not . equals`, no?
notEquals :: Eq a => Set a -> Set a -> Bool
notEquals (Set xs) (Set ys) = any (`notElem` ys) xs || any (`notElem` xs) ys

-- | Returns an empty set
emptySet :: Set a
emptySet = Set []
