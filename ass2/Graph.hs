module Graph (
  Graph,
  Path,

-- API functions:
  makeGraph,
  predecessors,
  successors,
  isConnected,
  findPath,
  -- findPathLabel,
  -- findMinCostPath,
  -- findPathWithLabel,

-- Additional functions:
  -- mst
) where

-- a note, for testing in console.
-- let s = makeGraph ("abcdefg", [('a',2,'b'),('b',2,'d'),('a',4,'g'),('b',3,'c'),('c',3,'d'),('c',5,'e'),('g',6,'c'),('c',5,'f'),('f',5,'e'),('c',6,'a')])

import OlSet as Set
import Data.Maybe

{---------------------------------------------
               Type declaration
---------------------------------------------}

type Arc a b = (a, b, a)

type Graph a b = (Set a, Set (Arc a b))

type Path a b = [Arc a b]

-- An internal structure to store information needed in path finding algorithm
data DijkstraState a b =
  DijkstraState {
    visitedArcs :: Set (Arc a b),
    cost         :: b
  }

{---------------------------------------------
                API Functions
---------------------------------------------}

-- | Create a graph with given lists of vertices and Arcs. Will raise an error
--   if any Arc has a start or end vertex which is not in the given list of
--   vertices, or if there are repeated vertices or Arcs.
makeGraph :: (Ord a, Ord b) => ([a], [Arc a b]) -> Graph a b
makeGraph (vs, as)
  | length vs > card vsSet        = error "Repeated vertices!"
  | length as > card asSet        = error "Repeated Arcs!"
  | hasUnseenVertex vsList asList = error "Found vertex not in the given list of vertices!"
  | otherwise                     = (vsSet, asSet)
  where vsSet  = makeSet vs
        asSet  = makeSet as
        vsList = orderedList vsSet
        asList = orderedList asSet

-- | Return the set of predecessors of a vertex (in a given graph – this will be
--   left implicit in describing subsequent functions), i.e. the set of all vertices
--   u, such that there is an Arc from u to the given vertex.
predecessors :: Ord a => Graph a b -> a -> Set a
predecessors (_, as) v = makeSet $ foldr f [] (orderedList as)
    where f (m,_,n) result = if n == v then m : result else result

-- | Return the set of successors of a vertex, i.e. the set of all vertices v,
--   such that there is an Arc from the given vertex to v.
successors :: Ord a => Graph a b -> a -> Set a
successors (_, as) v = makeSet $ foldr f [] (orderedList as)
    where f (m,_,n) result = if m == v then n : result else result

-- | Check whether a graph is connected, relative to a given start vertex, i.e.
--   if there is a directed path from a given vertex to every other vertex.
isConnected :: (Ord a, Eq b) => Graph a b -> a -> Bool
isConnected graph@(vs,_) vertex = all (isJust . findPath graph vertex) (orderedList vs)

-- | Find a path from one given vertex to another, if there is one. If there is
--   more than one path from u to v, then findPath g u v will search by order of
--   arcs (triples), e.g. ('a',1,'b') will be searched before ('a',1,'c')
findPath :: (Ord a, Eq b) => Graph a b -> a -> a -> Maybe (Path a b)
findPath (vs,as) origin dest
  | not $ has origin vs = Nothing
  | not $ has dest vs   = Nothing
  | origin == dest      = Just []  -- this guard feels questionable, Just [] or Nothing???
  | otherwise           = maybeHead $ allPaths (orderedList as) [origin] origin dest

-- -- | Find the label on a path from one given vertex to another, if there is one.
-- --   The label on a path is the list of labels on the Arcs in the path. If there
-- --   is more than one path from u to v, then findPathLabel g u v (may return the label on any one of them.)
-- --   TODO: update the description in brackets to make it definitive
-- findPathLabel :: Graph a b -> a -> a -> Maybe [b]





-- -- | Find a path with minimal cost from one given vertex to another, if there is
-- --   one. If there is more than one path from u to v, with minimal cost then
-- --   findMinCostPath g u v (may return any one of them.)
-- --   TODO: update the description in brackets to make it definitive
-- findMinCostPath :: Graph a b -> a -> a -> Maybe Path a b





-- -- | Find a path starting from a given vertex with a given label. If there is
-- --   more than one path starting from u with label s, then findPathWithLabel g u s
-- --   (may return any one of them.)
-- --   TODO: update the description in brackets to make it definitive
-- findPathWithLabel :: Graph a b -> a -> [b] -> Maybe Path a b






{---------------------------------------------
            Additional functions
---------------------------------------------}

-- -- | Construct a minimal cost spanning tree for a given graph, where Tree a b is
-- --   a type consisting of trees with vertices of type a and labels of type b on
-- --   its Arcs.
-- --   TODO: This function is using (Prim's / Kruskal's) algorithm
-- mst :: Graph a b -> Tree a b



-- | Check if a vertex is reachable from a graph
isReachable :: Eq a => Graph a b -> a -> Bool
isReachable (_, as) v = any (\(m,_,n) -> m == v || n == v) (orderedList as)


{---------------------------------------------
               Internal functions
---------------------------------------------}

-- | Find all possible paths from origin to dest. It keeps track of visited
--   vertices along the way. This solution is inspired by "99 Haskell Problems"
--   https://wiki.haskell.org/99_questions/80_to_89
allPaths :: (Ord a, Eq b) => [Arc a b] -> [a] -> a -> a -> [Path a b]
allPaths arcs visitedVetices origin dest
  | origin == dest = [[]]
  | otherwise =
      [ arc:path | arc <- arcs,
                   start arc == origin,
                   let origin' = end arc,
                   origin' `notElem` visitedVetices,
                   let arcs' = [ arc' | arc' <- arcs, arc' /= arc ],
                   let visitedVetices' = end arc : visitedVetices,
                   path <- allPaths arcs' visitedVetices' origin' dest ]


-- | Test whether a list contains a given element using binary search.
--   N.B. the given list has to be sorted, otherwise the result is chaotic.
-- binarySearch :: Ord a => a -> [a] -> Bool
-- binarySearch _ [] = False
-- binarySearch x xs
--   | x == xs !! mid = True
--   | x <  xs !! mid = binarySearch x left
--   | otherwise      = binarySearch x right
--   where mid           = length xs `div` 2
--         (left, right) = (take mid xs, drop (mid + 1) xs)

-- | Quick sort. From https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
-- quicksort :: Ord a => [a] -> [a]
-- quicksort []     = []
-- quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
--   where (lesser, greater) = (filter (< p) xs, filter (>= p) xs)

hasUnseenVertex :: (Ord a, Ord b) => [a] -> [Arc a b] -> Bool
hasUnseenVertex _ []                       = False
hasUnseenVertex vs ((m,_,e):as)
  | m `notElem` vs || e `notElem` vs = True
  | otherwise                              = hasUnseenVertex vs as

-- | Return the set of predecessor Arcs of a vertex, i.e. the set of all Arcs
--   who goes into the given vertex.
predecessorArcs :: (Ord a, Ord b) => Graph a b -> a -> Set (Arc a b)
predecessorArcs (_, as) v = makeSet $ foldr f [] (orderedList as)
    where f e@(_,_,n) result = if n == v then e : result else result

-- | Return the set of successors Arcs of a vertex, i.e. the set of all Arcs
--   who comes out of the given vertex.
successorArcs :: (Ord a, Ord b) => Graph a b -> a -> Set (Arc a b)
successorArcs (_, as) v = makeSet $ foldr f [] (orderedList as)
    where f e@(m,_,_) result = if m == v then e : result else result

-- | Getter method. Return the start of the given Arc.
start :: Arc a b -> a
start (u,_,_) = u

-- | Getter method. Return the end of the given Arc.
end :: Arc a b -> a
end (_,_,v) = v

-- | Getter method. Return the label of the given Arc.
label :: Arc a b -> b
label (_,c,_) = c

-- | Return an initial fringe for searching with Dijkstra algorithm
initialState :: (Ord a, Ord b, Num b) => DijkstraState a b
initialState = DijkstraState emptySet 0

-- | An Maybe version of head. If the list is not empty, return the head of it,
--   otherwise return Nothing.
maybeHead :: [a] -> Maybe a
maybeHead lst = if null lst then Nothing else Just (head lst)
