module Tree where

import Data.List(minimumBy)

data Tree a = Node Int a [Tree a] | Leaf Int a deriving (Show)

minCostPath :: Tree a -> (Int, [a])
minCostPath (Leaf cost label) = (cost, [label])
minCostPath (Node c l subs)
  = minimumBy (\(cost1, _) (cost2, _) -> compare cost1 cost2) minCostPathOfSubs
    where minCostPathOfSubs = map (\(cost, labels) -> (cost + c, l : labels)) minCostPathOfSubs'
          minCostPathOfSubs' = map minCostPath subs

countLeaves :: Tree a -> Int
countLeaves (Leaf _ _) = 1
countLeaves (Node _ _ trees) = sum $ map countLeaves trees

maxHeight :: Tree a -> Int
maxHeight (Leaf _ _) = 1
maxHeight (Node _ _ subs) = 1 + maximum (map maxHeight subs)

tree :: Tree Int
tree = Node 0 1 [
         Leaf 99 99,
         Node 2 2 [
           Leaf 6 6,
           Leaf 7 7,
           Leaf 5 5,
           Node 3 3 [
             Leaf 1 1,
             Leaf 2 2
           ]
         ],
         Leaf 6 6,
         Node 2 2 [
           Leaf 8 8,
           Leaf 20 20,
           Node 2 2 [
             Leaf 2 2,
             Leaf 3 3
           ],
           Node 2 2 [
             Leaf 10 10,
             Node 1 1 [
               Leaf 4 4
             ]
           ]
         ]
       ]
