Discuss how the use of lazy evaluation affects the time cost of this implementation,
compared with the cost you would expect if eager evaluation was used.

-- The `perms` function will generate permutations of the given list, and `asc`
function will check whether this permutation is ascending order. Since the evaluation
in Haskell is lazy, plus the help of currying and function composition, `perms`
and `asc` work simultaneously, and this silly sort approach will stop right after
an ascending permutation is found. In the best case scenario where the first
permutation is in ascending order, the time cost is only O(1). However, in the
worst case scenario where the ascending ordered permunation is returned at last,
the cost is O(N^2).

If eager evaluation was used, the cost would always be O(N^2), because all permutations
would have to be generated first before the order was checked.

(But this silly sort approach is still extremely slow :( )

-----------------------------------------------------
                    Module APIs
-----------------------------------------------------

> module SillySort (
>   sort,
>   perms,
>   asc
> ) where

> -- for testing
> import qualified Data.List (sort)

> -- | An interesting approach of sort. What it does is iterating through all
> --   permutations until it finds a list that is in non-descending order.
> sort :: Ord a => [a] -> [a]
> sort l = head [s | s <- perms l, asc s]

perms:
Recursively generate permutations. The exit condition is empty list. In this sense
a permutation of that list is an element concatenating the permutations of the
rest of the list.

> -- | Returns a list of permutations of a given list
> perms :: Ord a => [a] -> [[a]]
> perms [] = [[]]
> perms xs = [y:ys | y <- xs, ys <- perms (delelteFirst y xs)]

asc:
`asc` function is pretty straight forward. Check each two elements in linear traversal.

> -- | Determins whether a list is in non-descending order
> asc :: Ord a => [a] -> Bool
> asc []  = True
> asc [_] = True
> asc (x:l@(y:_))
>   | x > y     = False
>   | otherwise = asc l

-----------------------------------------------------
                 Internal functions
                   (not exported)
-----------------------------------------------------

delelteFirst:
delete the first occurrence of x from a list

> -- | Delete the first occurrence of x from a list
> delelteFirst :: Eq a => a -> [a] -> [a]
> delelteFirst _ [] = []
> delelteFirst x (y:ys)
>   | x == y = ys
>   | otherwise = y : delelteFirst x ys

-----------------------------------------------------
                     Test cases
-----------------------------------------------------

One test to test them all!

> theTest :: Bool
> theTest = all (== True) [test_asc, test_perms, test_sort]

Test cases for `asc`:

> test_asc :: Bool
> test_asc = all (==True) [t1, t2, t3, t4, t5]
>   where t1 = asc [1,1,2,3,4,4,4,4,5,5,5,5,5,6,7,89,10000]
>         t2 = asc [5]
>         t3 = asc [5,5,5,5,5]
>         t4 = asc [1..5]
>         t5 = not $ asc [4,2,1]

Test cases for `perms`:

> test_perms :: Bool
> test_perms = Data.List.sort (perms "abc") == Data.List.sort ["abc","acb", "bac","bca","cab","cba"]

Test cases for `sort`:

> test_sort :: Bool
> test_sort = all (==True) [t1, t2, t3]
>   where t1 = sort [4,3,2,1,5] == [1..5]
>         t2 = sort [5,5,5,5,5] == [5,5,5,5,5]
>         t3 = sort "hellohello" == Data.List.sort "hellohello"
