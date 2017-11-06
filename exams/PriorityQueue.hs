module PriorityQueue where

import Data.List(sort)

type Priority = Int

newtype Pair a = Pair (a, Priority) deriving (Show)

instance Eq (Pair a) where
  (Pair (_, p1)) == (Pair (_, p2)) = p1 == p2

instance Ord (Pair a) where
  compare (Pair (_, p1)) (Pair (_, p2)) = compare p1 p2

newtype PriorityQueue a = PriorityQueue [Pair a] deriving (Show)

makePQ :: [Pair a] -> PriorityQueue a
makePQ list = PriorityQueue $ sort list

emptyPQ :: PriorityQueue a
emptyPQ = makePQ []

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PriorityQueue []) = True
isEmptyPQ (PriorityQueue _) = False

addPQ :: a -> Priority -> PriorityQueue a -> PriorityQueue a
addPQ = enq

getPQ :: PriorityQueue a -> (Maybe a, PriorityQueue a)
getPQ = deq

enq :: a -> Priority -> PriorityQueue a -> PriorityQueue a
enq item p (PriorityQueue list) = PriorityQueue $ sort $ Pair (item, p) : list

deq :: PriorityQueue a -> (Maybe a, PriorityQueue a)
deq pq@(PriorityQueue []) = (Nothing, pq)
deq (PriorityQueue list) = (Just item, PriorityQueue $ init list)
  where Pair (item, _) = last list

elts :: PriorityQueue a -> [a]
elts (PriorityQueue list) = map extract list
  where extract (Pair (item, _)) = item
