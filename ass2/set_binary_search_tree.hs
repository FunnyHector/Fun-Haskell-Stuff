-- Represented using binary search tree
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
  select',

-- Additional functions:
  difference,
  toSortedList,
  toInOrderList,
  isEmpty,
  mapSet,
  partition,
  foldSet,
  identicalTree
) where

{---------------------------------------------
               Type declaration
---------------------------------------------}

data Tree a = Branch { label :: a, left :: Tree a, right :: Tree a }
            | Null
            deriving (Show)

type Set a = Tree a

instance Ord a => Eq (Tree a) where
  (==) = equals

-- Making the tree Foldable is to enable us to build a sorted list using cons
-- operator (:) and post-order traversal
instance Foldable Tree where
  foldr _ identity Null = identity
  foldr fun identity (Branch x l r) = foldr fun (fun x (foldr fun identity r)) l
  foldl _ identity Null = identity
  foldl fun identity (Branch x l r) = foldl fun (fun (foldl fun identity l) x) r

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

-- | Test whether a Set contains a given element.
has :: Ord a => a -> Set a -> Bool
has _ Null = False
has x (Branch y l r)
  | x == y    = True
  | x <  y    = has x l
  | otherwise = has x r

-- | Find the cardinality (number of elements) of a Set
card :: Set a -> Int
card Null = 0
card (Branch _ l r) = 1 + card l + card r

-- | Add an element to a Set, leaving the Set unchanged if it is already there
add :: Ord a => a -> Set a -> Set a
add x Null = Branch x Null Null
add x (Branch y l r)
  | x == y    = Branch y l r
  | x <  y    = Branch y (add x l) r
  | otherwise = Branch y l (add x r)

-- | Delete an element from a Set, leaving the Set unchanged if it is not there
del :: Ord a => a -> Set a -> Set a
del _ Null = Null
del x (Branch y l r)
  | x <  y    = Branch y (del x l) r
  | x >  y    = Branch y l (del x r)
  | otherwise = delNode (Branch y l r)

-- | Form the union of two Sets, i.e. the set of elements that occur in either
--   (or both) of the sets
union :: Ord a => Set a -> Set a -> Set a
union Null set    = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
union set Null    = set
union treeA treeB = foldr add treeA (toInOrderList treeB)

-- | Form the intersection of two sets, i.e. the set of all elements that occur
--   in both sets.
intersect :: Ord a => Set a -> Set a -> Set a
intersect Null _      = Null  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
intersect _ Null      = Null
intersect treeA treeB = makeSet $ intersectList (toInOrderList treeA) (toInOrderList treeB)

-- | Determine whether two sets are equal, i.e. whether every element that occurs
--   in either of the sets also occurs in the other.
equals :: Ord a => Set a -> Set a -> Bool
equals treeA treeB = toSortedList treeA == toSortedList treeB

-- | Determine whether one set is contained in another, i.e. whether every element
--   that occurs in the first set also occurs in the second.
subset :: Ord a => Set a -> Set a -> Bool
subset treeA treeB = all (`has` treeB) (toSortedList treeA)

-- | Return the set of elements of a given set satisfying a given property
--   This approach construct a new tree
select :: Ord a => (a -> Bool) -> Set a -> Set a
select f tree = makeSet $ filter f (toInOrderList tree)

-- | Return the set of elements of a given set satisfying a given property
--   This approach modifies the given tree (by deleting nodes that do not satisfy
--   the function)
select' :: Ord a => (a -> Bool) -> Set a -> Set a
select' _ Null = Null
select' f (Branch x l r)
  | f x       = Branch x (select' f l) (select' f r)
  | otherwise = select' f newBranch
  where newBranch = delNode (Branch x l r)

{---------------------------------------------
            Additional functions
        (not required in the handout)
---------------------------------------------}

-- | Form the difference of two sets, i.e. the set of elements that occur only in
--   the first set but not in the second set.
difference :: Ord a => Set a -> Set a -> Set a
difference set Null    = set
difference Null _      = Null
difference treeA treeB = makeSet $ filter (`notElem` listB) listA
  where listA = toInOrderList treeA
        listB = toInOrderList treeB

-- | Convert the set to a sorted list.
toSortedList :: Ord a => Set a -> [a]
toSortedList = foldr (:) []
-- toSortedList = foldl (\i e -> i ++ [e]) []   -- if fold from left, it would be much less efficient

-- | Perform a depth first search, and convert the set to a list.
--   N.B. we can apply `makeSet` on the returned list to construct an identical tree.
toInOrderList :: Ord a => Set a -> [a]
toInOrderList Null = []
toInOrderList (Branch x l r) = toInOrderList l ++ toInOrderList r ++ [x]

-- | Determine whether the set is empty
isEmpty :: Set a -> Bool
isEmpty Null = True
isEmpty _    = False

-- | Return a Set obtained by applying function f to each element of the given
--   Set. Note that the size of returned set is less or equal to the size of
--   original set.
mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet f = makeSet . map f . toInOrderList

-- | Partition the set using the given predicate, and return two sets: the first
--   one has all elements satisfying the predicate, and the second one has the
--   rest.
partition :: Ord a => (a -> Bool) -> Set a -> (Set a,Set a)
partition f tree = (makeSet $ filter f list, makeSet $ filter (not . f) list)
  where list = toInOrderList tree

-- | Fold the elements in the set using the given binary operator.
--   N.B. internally this is applying `foldr` on the ordered list, so the given
--   function should be (\element, identity -> element `f` identity)
foldSet :: (Ord a, Ord b) => (a -> b -> b) -> b -> Set a -> b
foldSet = foldr

-- | Check if two sets have identical trees backed.
identicalTree :: Ord a => Set a -> Set a -> Bool
identicalTree Null Null      = True
identicalTree Null Branch {} = False
identicalTree Branch {} Null = False
identicalTree (Branch x1 l1 r1) (Branch x2 l2 r2)
  | x1 /= x2  = False
  | otherwise = (==) l1 l2 && (==) r1 r2

{---------------------------------------------
               Internal functions
---------------------------------------------}

-- | Delete the node from the tree. This helper function only works if
delNode :: Ord a => Set a -> Set a
delNode Null                 = Null -- this should never be triggered
delNode (Branch _ Null Null) = Null
delNode (Branch _ l Null)    = l
delNode (Branch _ Null r)    = r
delNode (Branch _ l r)       = let m = maximal l in Branch m (del m l) r

maximal :: Ord a => Set a -> a
maximal Null              = error "Wrong usage! Can't apply on empty tree"
maximal (Branch x _ Null) = x
maximal (Branch _ _ r)    = maximal r

-- | Intersect two lists
intersectList :: Ord a => [a] -> [a] -> [a]
intersectList xs ys = foldr (\e result -> if e `elem` xs && e `elem` ys then e:result else result) [] ys
