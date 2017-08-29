Some general design decisions:

1. Type synonym or algebraic data type?
-- I decide to use algebraic data type for `Set`, because stricktly speaking a
set is not same as a list. A type synonym would be inaccurate. Also by creating
new data type, I have the power to override type class functions like what I did
with `==` and `/=`. I'm not sure but I don't think we can override functions if
it's just a type synonym.

2. "Can you use (==) instead of equals as the name for function `equals`?"
-- Yes, we can. And in my opinion we should, because `equals` and `(==)` are
semantically identical. So I overrode the `(==)` function with `equals`. They are
just alias to each other and now we can conveniently use `==` to compare the
equivalence of two sets.

3. "Can you use (<=) instead of subset as the name for this function?"
-- No, I don't think we should. I decide to not put `Set` under `Ord` class, and
not implement the `(<=)`, `(<)`, `(>=)`, or `(>)` function. In my opinion, sets
cannot and should not be ordered. Being a subset to another and being less or
equal to another are two different things. If we do want a definition of infix
operator for subset, I would suggest using another character(s), perhaps like
`<#` (if it's valid), to avoid ambiguity.

4. Smart constructors
-- The constructor is just taking a list (ordered or unordered), which means we
can construct a set with duplicate elements if we want. To enforce the behaviour
of not allowing duplicates, I decide to hide the constructors from exporting, so
that to construct a Set one has to use the function `makeSet` instead of directly
call the constructor. (See "Smart constructors": wiki.haskell.org/Smart_constructors)

5. Notable differences between OLSet (Set backed by ordered list) and ULSet (Set
backed by unordered list)
-- Most functions in these two Set modules are quite similar. But I've taken advantage
of ordered list in OLSet by doing binary search wherever I need to search through
the list. In theory OLSet should be faster to query, but slower to modify.
Another difference is that in OLSet I use `Ord a` as the type constraint for lots
of functions while in ULSet I use `Eq a`. This is to enable the elements to be
sorted and hence maintain ordered.

-----------------------------------------------------
                    Module APIs
-----------------------------------------------------

> -- Represented using unordered list
> module ULSet (
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
>
> -- Additional functions:
>   emptySet,
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

> newtype Set a = Set { unorderedList :: [a] } deriving (Show)
>
> instance Eq a => Eq (Set a) where
>   (==) = equals
>   (/=) = notEquals

-----------------------------------------------------
                 Required Functions
-----------------------------------------------------

> -- | Create a Set with a given list of values, ignoring repeated elements.
> makeSet :: Eq a => [a] -> Set a
> makeSet xs = Set $ uniqueList xs

> -- | Test whether a Set contains a given element
> has :: Eq a => a -> Set a -> Bool
> has x (Set xs) = x `elem` xs

> -- | Find the cardinality (number of elements) of a Set
> card :: Set a -> Int
> card (Set xs) = length xs

> -- | Add an element to a Set, leaving the Set unchanged if it is already there
> add :: Eq a => a -> Set a -> Set a
> add x (Set xs)
>   | x `elem` xs = Set xs
>   | otherwise   = Set (x:xs)   -- add the element at the head. Adding in front is faster than at the end, because `:` is more efficient than `++`

> -- | Delete an element from a Set, leaving the Set unchanged if it is not there
> del :: Eq a => a -> Set a -> Set a
> del x (Set xs)
>   | x `elem` xs = Set (filter (/=x) xs)
>   | otherwise   = Set xs

union:
  1. concatenate two lists together
  2. apply `uniqueList` function to make each element unique
  3. make the set with the unique list (and do not care about the order)

> -- | Form the union of two Sets, i.e. the set of elements that occur in either
> --   (or both) of the sets
> union :: Eq a => Set a -> Set a -> Set a
> union (Set []) set      = set  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> union set (Set [])      = set
> union (Set xs) (Set ys) = Set (uniqueList $ xs ++ ys)

intersect:
  1. intersect two lists, i.e. pick up only elements that occur in both lists
  2. apply `uniqueList` function to make each element unique
  3. make the set with the unique list (and do not care about the order)

> -- | Form the intersection of two sets, i.e. the set of all elements that occur
> --   in both sets.
> intersect :: Eq a => Set a -> Set a -> Set a
> intersect (Set []) _        = emptySet  -- the 1st & 2nd patterns aren't necessary, but could improve efficiency, eh?
> intersect _ (Set [])        = emptySet
> intersect (Set xs) (Set ys) = Set (uniqueList $ intersectList xs ys)

equals:
since two internal lists are both unordered, it's not easy to directly compare
two lists. We use `subset` for checking instead. The idea here is: if two sets
are subset of each other, they contain same elements.

> -- | Determine whether two sets are equal, i.e. whether every element that occurs
> --   in either of the sets also occurs in the other.
> equals :: Eq a => Set a -> Set a -> Bool
> equals setA setB = subset setA setB && subset setB setA

> -- | Determine whether one set is contained in another, i.e. whether every element
> --   that occurs in the first set also occurs in the second.
> subset :: Eq a => Set a -> Set a -> Bool
> subset (Set xs) (Set ys) = all (`elem` ys) xs

> -- | Return the set of elements of a given set satisfying a given property
> select :: Eq a => (a -> Bool) -> Set a -> Set a
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
> difference :: Eq a => Set a -> Set a -> Set a
> difference set (Set [])      = set
> difference (Set []) _        = emptySet
> difference (Set xs) (Set ys) = Set (filter (`notElem` ys) xs)

toList:
Simply returns the backing list.
This function remains questionable because theoretically:
  if setA == setB
  then toList setA == toList setB
This should be enforced especially in functional programming language. But we
can't guarantee this, because in ULSet module, Set is backed by unordered list,
and setA == setB can't guarantee that the backing lists have same order.

> -- | Convert the set to a list. No order guaranteed.
> toList :: Set a -> [a]
> toList = unorderedList  -- Lovin' record syntax!

> -- | Determine whether the set is empty
> isEmpty :: Set a -> Bool
> isEmpty set = card set == 0

mapSet:
This is similar to normal `map` function that applies to List, except that after
mapping, there could be duplicate elements. So what `mapSet` function does is:
  1. map the internal list
  2. make the mapped list unique
  3. make the set

> -- | Return a Set obtained by applying function f to each element of the given
> --   Set. Note that the size of returned set is less or equal to the size of
> --   original set.
> mapSet :: (Eq a, Eq b) => (a -> b) -> Set a -> Set b
> mapSet f (Set xs) = (makeSet . uniqueList . map f) xs

partition:
partition is pretty straightforward: filter by function f, and filter by function
(not.f)

> -- | Partition the set using the given predicate, and return two sets: the first
> --   one has all elements satisfying the predicate, and the second one has the
> --   rest.
> partition :: (a -> Bool) -> Set a -> (Set a,Set a)
> partition f (Set xs) = (Set $ filter f xs, Set $ filter (not . f) xs)

foldSet:
I guess normally we don't fold a set because in a fold process, order matters.
But I made this function anyway. What it does is apply `foldr` to the internal
list.
N.B. This function has the same problem as `toList` does. See discussion in `toList`.
On the other hand, the `foldSet` function can guarantee the deterministic result
in OLSet, simply because OLSet is backed by ordered list.

> -- | Fold the elements in the set using the given binary operator
> --   N.B. internally this is applying `foldr` on the unordered list, so the given
> --   function should be (\element, identity -> element `f` identity)
> foldSet :: (Eq a, Eq b) => (a -> b -> b) -> b -> Set a -> b
> foldSet f identity (Set xs) = foldr f identity xs

-----------------------------------------------------
                 Internal functions
                   (not exported)
-----------------------------------------------------

> -- | Make a list that only contains unique elements from a given list. Same as
> --   Data.List.nub.
> uniqueList :: Eq a => [a] -> [a]
> uniqueList = foldr (\e result -> if e `elem` result then result else e:result) []

> -- | Intersect two lists.
> intersectList :: Eq a => [a] -> [a] -> [a]
> intersectList xs ys = foldr (\e result -> if e `elem` xs && e `elem` ys then e:result else result) [] ys

notEquals:
`notEquals` can be implemented simply as `not . equals`. But I think by using
`any` function, this approach is faster, as we don't need to go through the whole
list.

> -- | Determine whether two sets are not equal.
> --   I think this approach would be faster than `not . equals`, no?
> notEquals :: Eq a => Set a -> Set a -> Bool
> notEquals (Set xs) (Set ys) = any (`notElem` ys) xs || any (`notElem` xs) ys

-----------------------------------------------------
                     Test cases
        (Same test cases in three Set Modules)
-----------------------------------------------------

Given that ULSet, OLSet, and BSTSet should behave exactly same externally, I used
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
> test_equals = all (==True) [t1, t2, t3]
>   where t1 = makeSet [3,3,1,5,2,2,3,4,1,1,4,5] == makeSet [1..5]
>         t2 = makeSet "qweqwewqewewewqweqweqweeeweqwe" == makeSet "qwe"
>         t3 = makeSet [makeSet [], makeSet [],makeSet [5,4..1], makeSet [1..5], makeSet [3,3,2,2,1,1,4,4,5,5]] == makeSet [emptySet, makeSet [1..5]]

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
