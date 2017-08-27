(Please see UlSet for some general discussion.)

-----------------------------------------------------
                    Module APIs
-----------------------------------------------------

> -- Represented using binary search tree
> module BstSet (
>   Set,   -- no data constructor exported. Have to make a Set using function `makeSet`.
>   makeSet,
>   has,
>   card,
>   add,
>   del,
>   union,
>   intersect,
>   equals,
>   subset,
>   select,
>   select',
>
> -- Additional functions:
>   emptySet,
>   difference,
>   toSortedList,
>   toInOrderList,
>   isEmpty,
>   mapSet,
>   partition,
>   foldSet,
>   identicalTree
> ) where

-----------------------------------------------------
                  Type declarations
-----------------------------------------------------

> data Tree a = Branch { label :: a, left :: Tree a, right :: Tree a }
>             | Null
>             deriving (Show)
>
> type Set a = Tree a
>
> instance Ord a => Eq (Tree a) where
>   (==) = equals
>
> -- Making the tree Foldable is to enable us to build a sorted list using cons
> -- operator (:) and post-order traversal
> instance Foldable Tree where
>   foldr _ identity Null = identity
>   foldr fun identity (Branch x l r) = foldr fun (fun x (foldr fun identity r)) l
>   foldl _ identity Null = identity
>   foldl fun identity (Branch x l r) = foldl fun (fun (foldl fun identity l) x) r

-----------------------------------------------------
                 Required Functions
-----------------------------------------------------

makeSet:
`makeSet` uses `add` function, to add each element from the given list, from right
to left. This will construct an unbalanced binary search tree, which implies that
if the given list is an ordered list, the tree will deteriorate to a linear linked
list, and the search cost is hence the worst case O(N).

> -- | Create a Set (build a binary search tree) with a given list of values,
> --   ignoring repeated elements. The elements in the given list are added from
> --   right to left.
> --   Note that this is only an implementation of unbalanced binary search tree,
> --   so it is subject to possible deterioration of linear search resulting O(n)
> --   complexity.
> makeSet :: Ord a => [a] -> Set a
> makeSet = foldr add Null

has:
Standard recursive traversal of binary search tree.

> -- | Test whether a Set contains a given element.
> has :: Ord a => a -> Set a -> Bool
> has _ Null = False
> has x (Branch y l r)
>   | x == y    = True
>   | x <  y    = has x l
>   | otherwise = has x r

> -- | Find the cardinality (number of elements) of a Set
> card :: Set a -> Int
> card Null = 0
> card (Branch _ l r) = 1 + card l + card r

> -- | Add an element to a Set, leaving the Set unchanged if it is already there
> add :: Ord a => a -> Set a -> Set a
> add x Null = Branch x Null Null
> add x (Branch y l r)
>   | x == y    = Branch y l r
>   | x <  y    = Branch y (add x l) r
>   | otherwise = Branch y l (add x r)

del:
Deletion in binary search tree is always the most complex operation. The real magic
is the `delNode` function. See `delNode` function for more information.

> -- | Delete an element from a Set, leaving the Set unchanged if it is not there
> del :: Ord a => a -> Set a -> Set a
> del _ Null = Null
> del x (Branch y l r)
>   | x <  y    = Branch y (del x l) r
>   | x >  y    = Branch y l (del x r)
>   | otherwise = delNode (Branch y l r)

union:
Union is not an operation that we often apply on trees. So What I did here is
convert the second tree into a list, and add elements of the list to the first
tree. To avoid building a heavily unbalanced tree, I use `toInOrderList` to
convert the tree. See `toInOrderList` function for more information.

> -- | Form the union of two Sets, i.e. the set of elements that occur in either
> --   (or both) of the sets
> union :: Ord a => Set a -> Set a -> Set a
> union Null set    = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> union set Null    = set
> union treeA treeB = foldr add treeA (toInOrderList treeB)

Intersect:
Again, I don't think intersect is an operation that we often apply on trees. Two
equivalent sets can be backed by two drastically different trees. My approach
here is:
  1. convert two trees into two lists using function `toInOrderList`
  2. intersect two lists (with the order retained)
  3. reconstruct a new tree from the intersected list using function `makeSet`

> -- | Form the intersection of two sets, i.e. the set of all elements that occur
> --   in both sets.
> intersect :: Ord a => Set a -> Set a -> Set a
> intersect Null _      = Null  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> intersect _ Null      = Null
> intersect treeA treeB = makeSet $ intersectList (toInOrderList treeA) (toInOrderList treeB)

equals:
The equality check is done by converting trees into sorted lists and then comparing
the sorted lists. I believe it's faster than checking each element against the
other tree.

> -- | Determine whether two sets are equal, i.e. whether every element that occurs
> --   in either of the sets also occurs in the other.
> equals :: Ord a => Set a -> Set a -> Bool
> equals treeA treeB = toSortedList treeA == toSortedList treeB

> -- | Determine whether one set is contained in another, i.e. whether every element
> --   that occurs in the first set also occurs in the second.
> subset :: Ord a => Set a -> Set a -> Bool
> subset treeA treeB = all (`has` treeB) (toSortedList treeA)

Select:
I provide two different approaches here.
  Approach 1 (`select`):
    1. convert the tree to a list,
    2. filter the list,
    3. reconstruct the tree.
  Approach 2 (`select'`):
    1. traverse the tree,
    2. delete every node that doesn't satisfy the function.
These two approaches produce different trees, but equivalent sets. I don't know
which one is faster to be honest. If I have to guess, I would say approach 1 is
more efficient. Pure guess.

> -- | Return the set of elements of a given set satisfying a given property
> --   This approach construct a new tree
> select :: Ord a => (a -> Bool) -> Set a -> Set a
> select f tree = makeSet $ filter f (toInOrderList tree)

> -- | Return the set of elements of a given set satisfying a given property
> --   This approach modifies the given tree (by deleting nodes that do not satisfy
> --   the function)
> select' :: Ord a => (a -> Bool) -> Set a -> Set a
> select' _ Null = Null
> select' f (Branch x l r)
>   | f x       = Branch x (select' f l) (select' f r)
>   | otherwise = select' f newBranch
>   where newBranch = delNode (Branch x l r)

-----------------------------------------------------
                 Additional functions
            (not required in the handout)
-----------------------------------------------------

> -- | Return an empty set
> emptySet :: Set a
> emptySet = Null

difference:
Total reconstruction of the tree. What it does:
  1. convert two trees into two lists using `toInOrderList`
  2. use standard library function `filter` to pick up only the elements that occur
     in the first list but not in the second.
  3. reconstruct the tree from the filtered list

> -- | Form the difference of two sets, i.e. the set of elements that occur only in
> --   the first set but not in the second set.
> difference :: Ord a => Set a -> Set a -> Set a
> difference set Null    = set
> difference Null _      = Null
> difference treeA treeB = makeSet $ filter (`notElem` listB) listA
>   where listA = toInOrderList treeA
>         listB = toInOrderList treeB

toSortedList:
Possibly useless function. Convert the set to a sorted list. See `toInOrderList`
for difference.

> -- | Convert the set to a sorted list.
> toSortedList :: Ord a => Set a -> [a]
> toSortedList = foldr (:) []
> -- toSortedList = foldl (\i e -> i ++ [e]) []   -- if fold from left, it would be much less efficient

toInOrderList:
This function is useful because when adding elements to the tree, we don't want
them coming in order. This function will return a list so that if we apply
`makeSet` on this list, we will get an identical tree.

> -- | Perform a depth first search, and convert the set to a list.
> --   N.B. we can apply `makeSet` on the returned list to construct an identical tree.
> toInOrderList :: Ord a => Set a -> [a]
> toInOrderList Null = []
> toInOrderList (Branch x l r) = toInOrderList l ++ toInOrderList r ++ [x]

> -- | Determine whether the set is empty
> isEmpty :: Set a -> Bool
> isEmpty Null = True
> isEmpty _    = False  -- faster than checking the size of it.

mapSet:
We can't simply map each element on the tree while keeping the shape of the tree,
because after the mapping there could be duplicates and damaged order. So this is
also one of many reconstruction functions in this Module.

> -- | Return a Set obtained by applying function f to each element of the given
> --   Set. Note that the size of returned set is less or equal to the size of
> --   original set.
> mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
> mapSet f = makeSet . map f . toInOrderList

partition:
Again, reconstruction :(

> -- | Partition the set using the given predicate, and return two sets: the first
> --   one has all elements satisfying the predicate, and the second one has the
> --   rest.
> partition :: Ord a => (a -> Bool) -> Set a -> (Set a,Set a)
> partition f tree = (makeSet $ filter f list, makeSet $ filter (not . f) list)
>   where list = toInOrderList tree

foldSet:
I put the Tree under Foldable class, so here foldSet is just an alias to foldr.

> -- | Fold the elements in the set using the given binary operator.
> --   N.B. internally this is applying `foldr` on the ordered list, so the given
> --   function should be (\element, identity -> element `f` identity)
> foldSet :: (Ord a, Ord b) => (a -> b -> b) -> b -> Set a -> b
> foldSet = foldr

identicalTree:
This function could be useful if we really want to know if two sets are backed by
two identical trees. Unfortunately this can't be used as an alias of `(==)`,
because having identical trees is sufficient but not necessary for having
equivalent sets.

> -- | Check if two sets have identical trees backed.
> identicalTree :: Ord a => Set a -> Set a -> Bool
> identicalTree Null Null      = True
> identicalTree Null Branch {} = False
> identicalTree Branch {} Null = False
> identicalTree (Branch x1 l1 r1) (Branch x2 l2 r2)
>   | x1 /= x2  = False
>   | otherwise = (==) l1 l2 && (==) r1 r2

-----------------------------------------------------
                 Internal functions
                   (not exported)
-----------------------------------------------------

delNode:
This function is the deletion of Node in binary search tree.

> -- | Delete the node from the tree. This helper function only works if
> delNode :: Ord a => Set a -> Set a
> delNode Null                 = Null -- this should never be triggered
> delNode (Branch _ Null Null) = Null
> delNode (Branch _ l Null)    = l
> delNode (Branch _ Null r)    = r
> delNode (Branch _ l r)       = let m = maximal l in Branch m (del m l) r

maximal:
This function finds the maximal value (i.e. the value of the rightmost node) in
a given tree.

> -- | finds the maximal value (i.e. the value of the rightmost node) in a given
> --   tree.
> maximal :: Ord a => Set a -> a
> maximal Null              = error "Wrong usage! Can't apply on empty tree"
> maximal (Branch x _ Null) = x
> maximal (Branch _ _ r)    = maximal r

> -- | Intersect two lists
> intersectList :: Ord a => [a] -> [a] -> [a]
> intersectList xs ys = foldr (\e result -> if e `elem` xs && e `elem` ys then e:result else result) [] ys

-----------------------------------------------------
                     Test cases
        (Same test cases in three Set Modules)
-----------------------------------------------------

Given that UlSet, OlSet, and BstSet should behave exactly same externally, I used
a same suite of test cases on all of them.

One test to test them all!

> theTest :: Bool
> theTest = all (== True) [test_has, test_card, test_add, test_del, test_union, test_intersect, test_equals, test_subset, test_select, test_difference, test_isEmpty, test_mapSet, test_partition]

Test cases for `has`:

> test_has :: Bool
> test_has = all (==True) [t1, t2, t3]
>   where t1 = has 3 $ makeSet [1..5]
>         t2 = (not . has 0) $ makeSet [1..5]
>         t3 = has 'b' $ makeSet "bravo"

Test cases for `card`:

> test_card :: Bool
> test_card = all (==True) [t1, t2, t3]
>   where t1 = card emptySet == 0
>         t2 = card (makeSet [3,3,1,5,2,2,3,4,1,1,4,5]) == 5
>         t3 = card (makeSet "bravo? bravo, bravo! bravo!!!!") == 9

Test cases for `add`:

> test_add :: Bool
> test_add = all (==True) [t1, t2, t3]
>   where t1 = add 5 emptySet == makeSet [5,5,5]
>         t2 = add 5 (makeSet [1..5]) == makeSet [1..5]
>         t3 = add 0 (makeSet [1..5]) == makeSet [0..5]

Test cases for `del`:

> test_del :: Bool
> test_del = all (==True) [t1, t2, t3]
>   where t1 = del 5 emptySet == emptySet
>         t2 = del 8 (makeSet [1..5]) == makeSet [1..5]
>         t3 = del 3 (makeSet [1..5]) == makeSet [1,2,4,5]

Test cases for `union`:

> test_union :: Bool
> test_union = all (==True) [t1, t2, t3]
>   where t1 = union emptySet (makeSet [1..5]) == makeSet [1..5]
>         t2 = union (makeSet [1..5]) emptySet == makeSet [1..5]
>         t3 = union (makeSet [1..10]) (makeSet [5..15]) == makeSet [1..15]

Test cases for `intersect`:

> test_intersect :: Bool
> test_intersect = all (==True) [t1, t2, t3]
>   where t1 = intersect emptySet (makeSet [1..5]) == emptySet
>         t2 = intersect (makeSet [1..5]) emptySet == emptySet
>         t3 = intersect (makeSet [1..10]) (makeSet [5..15]) == makeSet [5..10]

Test cases for `equals`:

> test_equals :: Bool
> test_equals = all (==True) [t1, t2]
>   where t1 = makeSet [3,3,1,5,2,2,3,4,1,1,4,5] == makeSet [1..5]
>         t2 = makeSet "qweqwewqewewewqweqweqweeeweqwe" == makeSet "qwe"

Test cases for `subset`:

> test_subset :: Bool
> test_subset = all (==True) [t1, t2, t3]
>   where t1 = subset (makeSet [1..5]) (makeSet [1..5])
>         t2 = subset (makeSet [1..5]) (makeSet [1..10])
>         t3 = not $ subset (makeSet [0..5]) (makeSet [1..10])

Test cases for `select`:

> test_select :: Bool
> test_select = all (==True) [t1, t2]
>   where t1 = select (>5) (makeSet [1..10]) == makeSet [6..10]
>         t2 = select odd (makeSet [1..10]) == makeSet [1,3..10]

Test cases for `difference`:

> test_difference :: Bool
> test_difference = all (==True) [t1, t2, t3, t4]
>   where t1 = difference (makeSet [1..5]) emptySet == makeSet [1..5]
>         t2 = difference emptySet (makeSet [1..5]) == emptySet
>         t3 = difference (makeSet [1..5]) (makeSet [1..5]) == emptySet
>         t4= difference (makeSet [1..10]) (makeSet [5..15]) == makeSet [1..4]

Test cases for `isEmpty`:

> test_isEmpty :: Bool
> test_isEmpty = all (==True) [t1, t2]
>   where t1 = isEmpty emptySet
>         t2 = not $ isEmpty (makeSet [1..5])

Test cases for `mapSet`:

> test_mapSet :: Bool
> test_mapSet = all (==True) [t1, t2, t3]
>   where t1 = mapSet (`mod` 2) (makeSet [1..10]) == makeSet [0,1]
>         t2 = mapSet (+2) (makeSet [1..10]) == makeSet [3..12]
>         t3 = mapSet (*2) emptySet == emptySet

Test cases for `partition`:

> test_partition :: Bool
> test_partition = all (==True) [t1, t2, t3]
>   where t1 = partition odd (makeSet [1..10]) == (makeSet [1,3..10], makeSet [2,4..10])
>         t2 = partition (>5) (makeSet [1..10]) == (makeSet [6..10], makeSet [1..5])
>         t3 = partition (== 100) (makeSet [1..10]) == (emptySet, makeSet [1..10])

N.B. I have no test cases for `makeSet`, and `foldSet`.
- `makeSet` as the constructor is hard to test.
- `foldSet` is a questionable function as discussed above, so I did not test it.
