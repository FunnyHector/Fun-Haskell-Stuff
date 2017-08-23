module Graph (
  Graph,
  Path,

-- API functions:
  -- makeGraph,
  -- predecessors,
  -- successors,
  -- isConnected,
  -- findPath,
  -- findPathLabel,
  -- findMinCostPath,
  -- findPathWithLabel,

-- Additional functions:
  -- mst
) where

import OlSet as Set

{---------------------------------------------
               Type declaration
---------------------------------------------}

type Edge a b = (a, b, a)

type Graph a b = (Set a, Set (Edge a b))

type Path a b = [Edge a b]

{---------------------------------------------
                API Functions
---------------------------------------------}

-- | Create a graph with given lists of vertices and edges. Will raise an error
--   if any edge has a start or end vertex which is not in the given list of
--   vertices, or if there are repeated vertices or edges.
-- makeGraph :: (Ord a, Ord b) => ([a], [Edge a b]) -> Graph a b




-- bad implementation. Don't think about only going though the list once.
-- try to make the code look nice. like 1 let's check if there are repeated vertices,
-- 2 let's check if there are repeated edges, 3 let's check if any edge has unlisted
-- vertices as start or end, 4 ok sanity check done let's make the graph.
-- makeGraph (vs, es) = makeGraph' vs' es'
--   where vs' = quicksort vs
--         es' = quicksort es
--         makeGraph' (v1:(v2:vz)) (e1@(u1,c1,v1):(e2@(u2,c2,v2):ez))
--           | v1 == v2 = error "Repeated vertices!"
--           | e1 == e2 = error "Repeated edges!"
--           | u1 `notElem` vs = error "" ++ u1 ++ " is not in the given vertices!"
--           | v1 `notElem` vs = error "" ++ v1 ++ " is not in the given vertices!"


-- -- | Return the set of predecessors of a vertex (in a given graph – this will be
-- --   left implicit in describing subsequent functions), i.e. the set of all vertices
-- --   u, such that there is an edge from u to the given vertex.
-- predecessors :: Graph a b -> a -> Set a





-- -- | Return the set of successors of a vertex, i.e. the set of all vertices v,
-- --   such that there is an edge from the given vertex to v.
-- successors :: Graph a b -> a -> Set a





-- -- | Check whether a graph is connected, relative to a given start vertex, i.e.
-- --   if there is a directed path from a given vertex to every other vertex.
-- isConnected :: Graph a b -> a -> Bool





-- -- | Find a path from one given vertex to another, if there is one. If there is
-- --   more than one path from u to v, then findPath g u v (may return any one of them.)
-- --   TODO: update the description in brackets to make it definitive
-- findPath :: Graph a b -> a -> a -> Maybe Path a b





-- -- | Find the label on a path from one given vertex to another, if there is one.
-- --   The label on a path is the list of labels on the edges in the path. If there
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
-- --   its edges.
-- --   TODO: This function is using (Prim's / Kruskal's) algorithm
-- mst :: Graph a b -> Tree a b







{---------------------------------------------
               Internal functions
---------------------------------------------}

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

-- | Quick sort. From https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
  where (lesser, greater) = (filter (< p) xs, filter (>= p) xs)

-- | Check whether a list contains duplicates.
-- containDuplicates :: Eq a => [a] -> Bool
-- containDuplicates (x:xs) = foldr (&&) False
