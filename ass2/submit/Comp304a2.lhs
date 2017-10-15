Name:              Fang Zhao (300364061)
Course Number:     COMP304
Assignment Number: 2
Question Number:   2 & 3

(PS: This file may look weird because I first submitted Silly Sort and Graph in
different files as two separate modules, but put them together after saw the new
submission requirement. Sorry if it hurts :P)

-----------------------------------------------------
                  Silly Sort Discussion
-----------------------------------------------------

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

> module Comp304a2 (
> ----------------- Silly Sort APIs -------------------
>   sort,
>   perms,
>   asc,
>
> -------------------- Graph APIs ---------------------
>   Graph,
>   Path,
>
> -- API functions:
>   makeGraph,
>   predecessors,
>   successors,
>   isConnected,
>   findPath,
>   findPathLabel,
>   findMinCostPath,
>   findPathWithLabel,
>
> -- Additional functions:
>   -- mst,   -- not implemented
>   isReachable,
>   predecessorArcs,
>   successorArcs
> ) where

-----------------------------------------------------
                    Import list
-----------------------------------------------------

> import OLSet as Set (Set, card, makeSet, orderedList, has)
> import Data.Maybe (isJust, isNothing)
> import Data.List (minimumBy, find)
> import qualified Data.List (sort)
> import Data.Ord (comparing)

-----------------------------------------------------
            Type declarations for Graph
-----------------------------------------------------

In addition to the two type synonyms provided in the handout, I added `Arc` to
represent an arc (directed edge).

> type Arc a b = (a, b, a)
>
> type Graph a b = (Set a, Set (Arc a b))
>
> type Path a b = [Arc a b]

-----------------------------------------------------
                 Functions for Silly Sort
-----------------------------------------------------

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
           Internal Functions for Silly Sort
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
              Test cases for Silly Sort
-----------------------------------------------------

One test to test them all!

> theTest1 :: Bool
> theTest1 = all (== True) [test_asc, test_perms, test_sort]

Test cases for `asc`:
Covers positive and negative paths.

> test_asc :: Bool
> test_asc = all (==True) [t1, t2, t3, t4, t5]
>   where t1 = asc [1,1,2,3,4,4,4,4,5,5,5,5,5,6,7,89,10000]
>         t2 = asc [5]
>         t3 = asc [5,5,5,5,5]
>         t4 = asc [1..5]
>         t5 = not $ asc [4,2,1]

Test cases for `perms`:
Covers the direct expectation of this function. I used `Data.List.sort` to help
lining all permutations up in order.

> test_perms :: Bool
> test_perms = Data.List.sort (perms "abc") == Data.List.sort ["abc","acb", "bac","bca","cab","cba"]

Test cases for `sort`:
Tests on different types.

> test_sort :: Bool
> test_sort = all (==True) [t1, t2, t3]
>   where t1 = sort [4,3,2,1,5] == [1..5]
>         t2 = sort [5,5,5,5,5] == [5,5,5,5,5]
>         t3 = sort "hellohello" == Data.List.sort "hellohello"

-----------------------------------------------------
                 Functions for Graph
-----------------------------------------------------

makeGraph:
  1. make a set of vertices, and a set of arcs.
  2. if the size of vertices set is less than the size of vertices list, raise
     error for repeated vertex.
  3. if the size of arcs set is less than the size of arcs list, raise error for
     repeated arcs.
  4. go through the arcs, check if any arc has an vertex that is not in the set
     of vertices. If so, raise error.
  5. make the graph.

> -- | Create a graph with given lists of vertices and Arcs. Will raise an error
> --   if any Arc has a start or end vertex which is not in the given list of
> --   vertices, or if there are repeated vertices or Arcs.
> makeGraph :: (Ord a, Ord b) => ([a], [Arc a b]) -> Graph a b
> makeGraph (vs, as)
>   | length vs > card vsSet         = error "Repeated vertices!"
>   | length as > card asSet         = error "Repeated Arcs!"
>   | hasUnknownVertex vsList asList = error "Found vertex not in the given list of vertices!"
>   | otherwise                      = (vsSet, asSet)
>   where vsSet  = makeSet vs
>         asSet  = makeSet as
>         vsList = orderedList vsSet
>         asList = orderedList asSet

predecessors:
Pretty straight forward. Fold through the arcs with a filter function. The filter
function picks the start vertex of any arc whose end vertex is the given vertex.

> -- | Return the set of predecessors of a vertex (in a given graph – this will be
> --   left implicit in describing subsequent functions), i.e. the set of all vertices
> --   u, such that there is an Arc from u to the given vertex.
> predecessors :: Ord a => Graph a b -> a -> Set a
> predecessors (_, as) v = makeSet $ foldr f [] (orderedList as)
>     where f (m,_,n) result = if n == v then m : result else result

successors:
Similar to `predecessors`. Fold through the arcs with a filter function. The filter
function picks the end vertex of any arc whose start vertex is the given vertex.

> -- | Return the set of successors of a vertex, i.e. the set of all vertices v,
> --   such that there is an Arc from the given vertex to v.
> successors :: Ord a => Graph a b -> a -> Set a
> successors (_, as) v = makeSet $ foldr f [] (orderedList as)
>     where f (m,_,n) result = if m == v then n : result else result

isConnected:
This function uses `findPath` on each vertex in the graph. If none of the results
is Nothing, then we know that there is a directed path from the given vertex to
every other vertex.

> -- | Check whether a graph is connected, relative to a given start vertex, i.e.
> --   if there is a directed path from a given vertex to every other vertex.
> isConnected :: (Ord a, Eq b) => Graph a b -> a -> Bool
> isConnected graph@(vs,_) vertex = all (isJust . findPath graph vertex) (orderedList vs)

findPath:
This function uses `allPaths` function, which returns all possible paths from
origin vertex to destination vertex. After getting the results from `allPaths`,
we wrap the result into either Nothing or Just (head allPaths), and return it as
the final result.
(See `allPaths` function for more information.)

(PS: One day before deadline, I found that the `find` function from Data.List is
SUPER useful for this function and will theoretically boost the performance because
`find` wraps Maybe for me. Unfortunately I just don't have time to refactor my
code Orz...)

> -- | Find a path from one given vertex to another, if there is one. If there is
> --   more than one path from u to v, then findPath g u v will search by order of
> --   arcs (triples), e.g. ('a',1,'b') will be searched before ('a',1,'c')
> findPath :: (Ord a, Eq b) => Graph a b -> a -> a -> Maybe (Path a b)
> findPath (vs,as) origin dest
>   | not $ has origin vs = Nothing
>   | not $ has dest vs   = Nothing
>   | origin == dest      = Just []  -- this guard feels questionable, Just [] or Nothing???
>   | otherwise           = maybeHead $ allPaths (orderedList as) [origin] origin dest
>   where maybeHead lst
>           | null lst  = Nothing
>           | otherwise = Just (head lst)

findPathLabel:
This function uses `findPath` function. If we get a Nothing result from `findPath`,
we return Nothing as well; if we get a Just (Path a b), we extract the label of
each arc using `map`, and return them.

> -- | Find the label on a path from one given vertex to another, if there is one.
> --   The label on a path is the list of labels on the Arcs in the path. If there
> --   is more than one path from u to v, then findPathLabel g u v  will search by
> --   order of arcs (triples), e.g. ('a',1,'b') will be searched before ('a',1,'c')
> findPathLabel :: (Ord a, Eq b) => Graph a b -> a -> a -> Maybe [b]
> findPathLabel graph origin dest = f $ findPath graph origin dest
>   where f Nothing   = Nothing
>         f (Just ls) = Just (map label ls)

findMinCostPath:
This function uses `allPaths` function. After getting all possible paths, we find
the one with minimum total cost.
(Dijkstra is just toooooooo hard for me :( Spent at least 8 hours on Dijkstra but
couldn't do it... Orz )

> -- | Find a path with minimal cost from one given vertex to another, if there is
> --   one. If there is more than one path from u to v, with minimal cost then
> --   findMinCostPath g u v will search by order of arcs (triples), e.g. ('a',1,'b')
> --   will be searched before ('a',1,'c')
> findMinCostPath :: (Ord a, Num b, Ord b ,Eq b) => Graph a b -> a -> a -> Maybe (Path a b)
> findMinCostPath (vs,as) origin dest
>   | not $ has origin vs = Nothing
>   | not $ has dest vs   = Nothing
>   | origin == dest      = Just []  -- this guard feels questionable, Just [] or Nothing???
>   | otherwise           = findMin $ allPaths (orderedList as) [origin] origin dest
>   where findMin paths
>           | null paths = Nothing
>           | otherwise  = Just (minimumBy (comparing totalCost) paths)
>           where totalCost = sum . map (\(_,c,_) -> c)

findPathWithLabel:
This function use `dfs` function, which is a depth-first search to generate all
available paths. I also use `find` function from Data.List package to find the
first path that matches the given list of labels, which also means that the `dfs`
will stop once `find` has a target, thanks to Haskell's lazy evaluation.

> -- | Find a path starting from a given vertex with a given label(s). If there is
> --   more than one path starting from u with label s, then findPathWithLabel g u s
> --   will search by order of arcs (triples), e.g. ('a',1,'b') will be searched
> --   before ('a',1,'c')
> findPathWithLabel :: (Ord a, Ord b) => Graph a b -> a -> [b] -> Maybe (Path a b)
> findPathWithLabel (vs,as) vertex labels
>   | not $ has vertex vs                     = Nothing
>   | hasUnknownlabel (orderedList as) labels = Nothing
>   | otherwise                               = find (hasLabels labels) (dfs (orderedList as) [] vertex)

-----------------------------------------------------
            Additional Functions for Graph
            (not required in the handout)
-----------------------------------------------------

mst:
Don't have time to finish it.
¯\_(ツ)_/¯

> -- | Construct a minimal cost spanning tree for a given graph, where Tree a b is
> --   a type consisting of trees with vertices of type a and labels of type b on
> --   its Arcs.
> --   TODO: This function is using (Prim's / Kruskal's) algorithm
> -- mst :: Graph a b -> Tree a b

isReachable:
This function checks if a vertex can be reached from a graph. It's like the
opposite of `isConnected`, because `isConnected` checks if a vertex can reach
to any other vertex.

> -- | Check if a vertex is reachable from a graph
> isReachable :: Eq a => Graph a b -> a -> Bool
> isReachable (_, as) v = any (\(m,_,n) -> m == v || n == v) (orderedList as)

predecessorArcs:
Like `predecessors`, but find all predecessor arcs.

> -- | Return the set of predecessor Arcs of a vertex, i.e. the set of all Arcs
> --   who goes into the given vertex.
> predecessorArcs :: (Ord a, Ord b) => Graph a b -> a -> Set (Arc a b)
> predecessorArcs (_, as) v = makeSet $ foldr f [] (orderedList as)
>     where f e@(_,_,n) result = if n == v then e : result else result

successorArcs:
Like `successors`, but find all successor arcs.

> -- | Return the set of successors Arcs of a vertex, i.e. the set of all Arcs
> --   who comes out of the given vertex.
> successorArcs :: (Ord a, Ord b) => Graph a b -> a -> Set (Arc a b)
> successorArcs (_, as) v = makeSet $ foldr f [] (orderedList as)
>     where f e@(m,_,_) result = if m == v then e : result else result

-----------------------------------------------------
             Internal functions for Graph
                   (not exported)
-----------------------------------------------------

allPaths:
This function is the key component of all path-finding related functions. It's
inspired by part 2 Silly Sort and "99 Haskell Problems"
(https://wiki.haskell.org/99_questions/80_to_89). Basically what this function
does is generating permutations of paths, and then selecting only the paths that
start from the origin and end at the destination.

(PS: Of cause the performance is super poor, but this is the best I can do :P)

> -- | Find all possible paths from origin to dest. It keeps track of visited
> --   vertices along the way. This solution is inspired by "99 Haskell Problems"
> --   https://wiki.haskell.org/99_questions/80_to_89
> allPaths :: (Ord a, Eq b) => [Arc a b] -> [a] -> a -> a -> [Path a b]
> allPaths arcs visitedVetices origin dest
>   | origin == dest = [[]]
>   | otherwise =
>       [ arc:path | arc <- arcs,
>                    start arc == origin,
>                    let origin' = end arc,
>                    origin' `notElem` visitedVetices,
>                    let arcs' = [ arc' | arc' <- arcs, arc' /= arc ],
>                    let visitedVetices' = end arc : visitedVetices,
>                    path <- allPaths arcs' visitedVetices' origin' dest ]

dfs:
A depth-first search. This is quite similar to allPaths. The difference is the
exit condition.

> -- | a Depth-first search to find all paths from the given vertex.
> dfs :: (Ord a, Eq b) => [Arc a b] -> [a] -> a -> [Path a b]
> dfs arcs visitedVetices origin
>   | noAvailableArcs = [[]]
>   | otherwise
>       = [ arc:path | arc <- arcs,
>                      start arc == origin,
>                      let origin' = end arc,
>                      let arcs' = [ arc' | arc' <- arcs, arc' /= arc ],
>                      let visitedVetices' = end arc : visitedVetices,
>                      path <- dfs arcs' visitedVetices' origin' ]
>    where noAvailableArcs = all (\(m,_,_) -> m /= origin) arcs

> -- | Given a list of vertices, and a list of arcs, check whether there is any
> --   arc that has either end not belonging to the list of vertices.
> hasUnknownVertex :: (Ord a, Ord b) => [a] -> [Arc a b] -> Bool
> hasUnknownVertex _ []                = False
> hasUnknownVertex vs ((m,_,e):as)
>   | m `notElem` vs || e `notElem` vs = True
>   | otherwise                        = hasUnknownVertex vs as

> -- | Given a list of arcs, and a list of labels, check whether there is any
> --   label that does not belong to any known arc.
> hasUnknownlabel :: (Ord a, Ord b) => [Arc a b] -> [b] -> Bool
> hasUnknownlabel _ []  = False
> hasUnknownlabel as ls = any (`notElem` knownLabels) ls
>   where knownLabels = map (\(_,l,_) -> l) as

hasLabels:
This function checks if a path has the given labels in correct order. Mainly
used by `findPathWithLabel`.

> -- | Check a path has the given labels in correct order.
> hasLabels :: (Ord a, Ord b) => [b] -> [Arc a b] -> Bool
> hasLabels [] _ = True
> hasLabels _ [] = False
> hasLabels x@(l:ls) ((_,k,_):as)
>   | l == k    = hasLabels ls as
>   | otherwise = hasLabels x as

> -- | Getter method. Return the start of the given Arc.
> start :: Arc a b -> a
> start (u,_,_) = u

> -- | Getter method. Return the end of the given Arc.
> end :: Arc a b -> a
> end (_,_,v) = v

> -- | Getter method. Return the label of the given Arc.
> label :: Arc a b -> b
> label (_,c,_) = c

-----------------------------------------------------
                 Test cases for Graph
-----------------------------------------------------

First let's create a test subject, a badass graph with self-pointing arcs,
cycles, and multi arcs between two vertices. This is the same graph in the forum
as I posted that question.

> -- | the test subject.
> graf :: Graph Char Int
> graf = makeGraph ("abcdefg", [('a',2,'b'),('b',2,'d'),('a',4,'g'),('b',3,'c'),('c',3,'d'),('c',20,'e'),('g',6,'c'),('c',5,'f'),('f',20,'e'),('c',6,'a'),('e',5,'e'),('a',4,'b'),('a',6,'b')])

One test to test them all!

> theTest2 :: Bool
> theTest2 = all (== True) [test_predecessors, test_successors, test_isConnected, test_findPath, test_findPathLabel, test_findMinCostPath]

Test cases for `predecessors`:
Tests on several typical vertices in the graph. Note that 'e' is a special case
because there is a self-pointing arc ('e',5,'e') in the graph, which means 'e' is
both predecessor and successor of itself.

> test_predecessors :: Bool
> test_predecessors = all (==True) [t1, t2, t3]
>   where t1 = predecessors graf 'c' == makeSet "bg"
>         t2 = predecessors graf 'd' == makeSet "bc"
>         t3 = predecessors graf 'e' == makeSet "ecf"

Test cases for `successors`:
Tests on several typical vertices in the graph. Note that 'e' is a special case
because there is a self-pointing arc ('e',5,'e') in the graph, which means 'e' is
both predecessor and successor of itself.

> test_successors :: Bool
> test_successors = all (==True) [t1, t2, t3]
>   where t1 = successors graf 'c' == makeSet "adef"
>         t2 = successors graf 'b' == makeSet "cd"
>         t3 = successors graf 'f' == makeSet "e"

Test cases for `isConnected`:
Covers both positive and negative paths.

> test_isConnected :: Bool
> test_isConnected = all (==True) [t1, t2, t3, t4]
>   where t1 = isConnected graf 'c'
>         t2 = not $ isConnected graf 'd'
>         t3 = isConnected graf 'g'
>         t4 = not $ isConnected graf 'f'

Test cases for `findPath`:
Covers four guards.

> test_findPath :: Bool
> test_findPath = all (==True) [t1, t2, t3, t4]
>   where t1 = findPath graf 'a' 'e' == Just [('a',2,'b'),('b',3,'c'),('c',5,'f'),('f',20,'e')]
>         t2 = findPath graf 'e' 'e' == Just []
>         t3 = isNothing $ findPath graf 'h' 'c'
>         t4 = isNothing $ findPath graf 'c' 'H'

Test cases for `findPathLabel`:
Covers four guards of `findPath`, since `findPathLabel` is a wrapper of `findPath`.

> test_findPathLabel :: Bool
> test_findPathLabel = all (==True) [t1, t2, t3, t4]
>   where t1 = findPathLabel graf 'a' 'e' == Just [2,3,5,20]
>         t2 = findPathLabel graf 'e' 'e' == Just []
>         t3 = isNothing $ findPathLabel graf 'h' 'c'
>         t4 = isNothing $ findPathLabel graf 'c' 'H'

Test cases for `findMinCostPath`:
Covers four guards.

> test_findMinCostPath :: Bool
> test_findMinCostPath = all (==True) [t1, t2, t3, t4]
>   where t1 = findMinCostPath graf 'a' 'e' == Just [('a',2,'b'),('b',3,'c'),('c',20,'e')]
>         t2 = findMinCostPath graf 'e' 'e' == Just []
>         t3 = isNothing $ findMinCostPath graf 'h' 'c'
>         t4 = isNothing $ findMinCostPath graf 'c' 'H'

No time to provide test cases for additional functions :P
¯\_(ツ)_/¯
