-- Represented using binary search tree
module Set (
  Set,   -- no data constructor exported. Have to make a Set using function `makeSet`.
  makeSet,
  -- has,
  -- card,
  add,
  -- del,
  -- union,
  -- intersect,
  -- equals,
  -- subset,
  -- select,

-- -- Additional functions:
--   difference,
--   toList,
--   isEmpty,
--   mapSet,
--   partition,
--   foldSet
) where

{---------------------------------------------
               Type declaration
---------------------------------------------}

data Tree a = Branch { label :: a, left :: Tree a, right :: Tree a }
            | Null

type Set a = Tree a

instance Show a => Show (Tree a) where
  show Null = "Null"
  show (Branch x l r) = "Branch { " ++ show x ++ ", left { " ++ show l ++ " }, right { " ++ show r ++ " } }"

-- instance Ord a => Eq (Set a) where
--   (==) = equals

{---------------------------------------------
                API Functions
---------------------------------------------}

-- | Create a Set (build a binary search tree) with a given list of values, ignoring
--   repeated elements. The elements in the given list are added from right to left.
--   Note that this is only an implementation of unbalanced binary search tree,
--   so it is subject to possible deterioration of linear search resulting O(n)
--   complexity.
makeSet :: Ord a => [a] -> Set a
makeSet = foldr add Null

-- -- | Test whether a Set contains a given element. This function is using binary
-- --   search.
-- has :: Ord a => a -> Set a -> Bool
-- has x (Set xs) = binarySearch x xs
--
-- -- | Find the cardinality (number of elements) of a Set
-- card :: Set a -> Int
-- card (Set xs) = length xs

-- | Add an element to a Set, leaving the Set unchanged if it is already there
add :: Ord a => a -> Set a -> Set a
add x Null = Branch x Null Null
add x (Branch y l r)
  | x == y    = Branch y l r
  | x <  y    = Branch y (add x l) r
  | otherwise = Branch y l (add x r)

-- -- | Delete an element from a Set, leaving the Set unchanged if it is not there
-- del :: Ord a => a -> Set a -> Set a
-- del x (Set xs)
--   | binarySearch x xs = Set (filter (/=x) xs)
--   | otherwise         = Set xs
--
-- -- | Form the union of two Sets, i.e. the set of elements that occur in either
-- --   (or both) of the sets
-- union :: Ord a => Set a -> Set a -> Set a
-- union (Set []) set      = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
-- union set (Set [])      = set
-- union (Set xs) (Set ys) = Set (orderedUniqueList $ xs ++ ys)
--
-- -- | Form the intersection of two sets, i.e. the set of all elements that occur
-- --   in both sets.
-- intersect :: Ord a => Set a -> Set a -> Set a
-- intersect (Set []) _        = emptySet  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
-- intersect _ (Set [])        = emptySet
-- intersect (Set xs) (Set ys) = Set (orderedUniqueList $ intersectList xs ys)
--
-- -- | Determine whether two sets are equal, i.e. whether every element that occurs
-- --   in either of the sets also occurs in the other.
-- equals :: Ord a => Set a -> Set a -> Bool
-- equals setA setB = subset setA setB && subset setB setA
--
-- -- | Determine whether one set is contained in another, i.e. whether every element
-- --   that occurs in the first set also occurs in the second.
-- subset :: Ord a => Set a -> Set a -> Bool
-- subset (Set xs) (Set ys) = all (`elem` ys) xs
--
-- -- | Return the set of elements of a given set satisfying a given property
-- select :: Ord a => (a -> Bool) -> Set a -> Set a
-- select f (Set xs) = Set (filter f xs)







{---------------------------------------------
               Internal functions
---------------------------------------------}

-- buildTree :: Ord a => [a] =>
