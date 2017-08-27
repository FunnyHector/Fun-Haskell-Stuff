(Please see UlSet for some general discussion.)

-----------------------------------------------------
                    Module APIs
-----------------------------------------------------

> -- Represented using ordered list
> module OlSet (
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

> -- Additional functions:
>   emptySet,
>   orderedList,
>   difference,
>   toList,
>   isEmpty,
>   mapSet,
>   partition,
>   foldSet
> ) where

-----------------------------------------------------
                  Type declarations
-----------------------------------------------------

> newtype Set a = Set { orderedList :: [a] } deriving (Show)
>
> instance Ord a => Eq (Set a) where
>   (==) = equals

-----------------------------------------------------
                 Required Functions
-----------------------------------------------------

> -- | Create a Set with a given list of values, ignoring repeated elements.
> makeSet :: Ord a => [a] -> Set a
> makeSet xs = Set $ orderedUniqueList xs

has:
For function `has`, I implemented a binary search function to take advantage of
ordered list. By doing this, the time taken for searching on large size set should
be greatly reduced from O(n) to O(log N).

> -- | Test whether a Set contains a given element. This function is using binary
> --   search.
> has :: Ord a => a -> Set a -> Bool
> has x (Set xs) = binarySearch x xs

> -- | Find the cardinality (number of elements) of a Set
> card :: Set a -> Int
> card (Set xs) = length xs

add:
  1. check if the set already has the element (using binary search).
  2. if it does, return the set itself
  3. if it doens't, add the element to the internal list, quick sort the list,
     and make the set with the sorted list.

> -- | Add an element to a Set, leaving the Set unchanged if it is already there
> add :: Ord a => a -> Set a -> Set a
> add x (Set xs)
>   | binarySearch x xs = Set xs
>   | otherwise         = Set $ quicksort (x:xs)

del:
  1. check if the set already has the element (using binary search).
  2. if it does, filter the element out, and make a set with the remaining list.
  3. if it doens't, return the set itself.

> -- | Delete an element from a Set, leaving the Set unchanged if it is not there
> del :: Ord a => a -> Set a -> Set a
> del x (Set xs)
>   | binarySearch x xs = Set (filter (/=x) xs)
>   | otherwise         = Set xs

union:
  1. concatenate two lists together
  2. apply `orderedUniqueList` function to make each element unique, and the list
     sorted.
  3. make the set with the sorted unique list.

> -- | Form the union of two Sets, i.e. the set of elements that occur in either
> --   (or both) of the sets
> union :: Ord a => Set a -> Set a -> Set a
> union (Set []) set      = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> union set (Set [])      = set
> union (Set xs) (Set ys) = Set (orderedUniqueList $ xs ++ ys)

intersect:
  1. intersect two lists, i.e. pick up only elements that occur in both lists
  2. apply `orderedUniqueList` function to make each element unique, and the list
     sorted.
  3. make the set with the sorted unique list.

> -- | Form the intersection of two sets, i.e. the set of all elements that occur
> --   in both sets.
> intersect :: Ord a => Set a -> Set a -> Set a
> intersect (Set []) _        = emptySet  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> intersect _ (Set [])        = emptySet
> intersect (Set xs) (Set ys) = Set (orderedUniqueList $ intersectList xs ys)

equals:
since two internal lists are both sorted, we can simply apply `(==)` to them to
check the equality.

> -- | Determine whether two sets are equal, i.e. whether every element that occurs
> --   in either of the sets also occurs in the other.
> equals :: Ord a => Set a -> Set a -> Bool
> equals setA setB = orderedList setA == orderedList setB

> -- | Determine whether one set is contained in another, i.e. whether every element
> --   that occurs in the first set also occurs in the second.
> subset :: Ord a => Set a -> Set a -> Bool
> subset (Set xs) (Set ys) = all (`elem` ys) xs

> -- | Return the set of elements of a given set satisfying a given property
> select :: Ord a => (a -> Bool) -> Set a -> Set a
> select f (Set xs) = Set (filter f xs)

-----------------------------------------------------
                 Additional functions
            (not required in the handout)
-----------------------------------------------------

> -- | Return an empty set
> emptySet :: Set a
> emptySet = Set []

> -- | Form the difference of two sets, i.e. the set of elements that occur only in
> --   the first set but not in the second set.
> difference :: Ord a => Set a -> Set a -> Set a
> difference set (Set [])      = set
> difference (Set []) _        = emptySet
> difference (Set xs) (Set ys) = Set (filter (`notElem` ys) xs)

> -- | Convert the set to an ordered list.
> toList :: Set a -> [a]
> toList = orderedList

> -- | Determine whether the set is empty
> isEmpty :: Set a -> Bool
> isEmpty set = card set == 0

mapSet:
This is similar to normal `map` function that applies to List, except that after
mapping, there could be duplicate elements, and the order may not be preserved.
So what `mapSet` function does is:
  1. map the internal list
  2. make the mapped list unique and sorted
  3. make the set

> -- | Return a Set obtained by applying function f to each element of the given
> --   Set. Note that the size of returned set is less or equal to the size of
> --   original set.
> mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
> mapSet f (Set xs) = (makeSet . orderedUniqueList . map f) xs

> -- | Partition the set using the given predicate, and return two sets: the first
> --   one has all elements satisfying the predicate, and the second one has the
> --   rest.
> partition :: (a -> Bool) -> Set a -> (Set a,Set a)
> partition f (Set xs) = (Set $ filter f xs, Set $ filter (not . f) xs)

foldSet:
Since OlSet is backed by ordered list, to fold the set is just to fold the internal
list. Note this function is using `foldr` on the list.
(See also the discussion of `foldSet` in UlSet module)

> -- | Fold the elements in the set using the given binary operator.
> --   N.B. internally this is applying `foldr` on the ordered list, so the given
> --   function should be (\element, identity -> element `f` identity)
> foldSet :: (Ord a, Ord b) => (a -> b -> b) -> b -> Set a -> b
> foldSet f identity (Set xs) = foldr f identity xs

-----------------------------------------------------
                 Internal functions
                   (not exported)
-----------------------------------------------------

> -- | Make a sorted list that only contains unique elements from a given list.
> --   Same as Data.List.sort . Data.List.nub
> orderedUniqueList :: Ord a => [a] -> [a]
> orderedUniqueList = quicksort . foldr (\e result -> if e `elem` result then result else e:result) []

quicksort:
This function is an entire quote from haskell wiki site. "The iconic quick sort
in Haskell".

> -- | Quick sort. From https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
> quicksort :: Ord a => [a] -> [a]
> quicksort []     = []
> quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
>   where (lesser, greater) = (filter (< p) xs, filter (>= p) xs)

binarySearch:
Theoritically, binary search on ordered list is O(log N), which is a lot better
than linear search (cost: O(N)). What I'm not sure about in my implementation is
the cost of `take` and `drop` functions. Would they offset the benefit of binary
search?

> -- | Test whether a list contains a given element using binary search.
> --   N.B. the given list has to be sorted, otherwise the result is chaotic.
> binarySearch :: Ord a => a -> [a] -> Bool
> binarySearch _ [] = False
> binarySearch x xs
>   | x == xs !! mid = True
>   | x <  xs !! mid = binarySearch x left
>   | otherwise      = binarySearch x right
>   where mid           = length xs `div` 2
>         (left, right) = (take mid xs, drop (mid + 1) xs)

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
