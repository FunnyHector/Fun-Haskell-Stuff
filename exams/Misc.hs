module Misc where

-------------- Binary search tree ----------------

data BinTree a = Empty | BinTree a (BinTree a) (BinTree a) deriving (Show)

add :: Ord a => a -> BinTree a -> BinTree a
add x Empty = BinTree x Empty Empty
add x (BinTree y l r)
  | x == y    = BinTree y l r
  | x <  y    = BinTree y (add x l) r
  | otherwise = BinTree y l (add x r)

del :: Ord a => a -> BinTree a -> BinTree a
del _ Empty = Empty
del x (BinTree y l r)
  | x <  y    = BinTree y (del x l) r
  | x >  y    = BinTree y l (del x r)
  | otherwise = delNode (BinTree y l r)

delNode :: Ord a => BinTree a -> BinTree a
delNode Empty                   = Empty -- this should never be triggered
delNode (BinTree _ Empty Empty) = Empty
delNode (BinTree _ l Empty)     = l
delNode (BinTree _ Empty r)     = r
delNode (BinTree _ l r)         = let m = maximal l in BinTree m (del m l) r

maximal :: Ord a => BinTree a -> a
maximal Empty               = error "Wrong usage! Can't apply on empty tree"
maximal (BinTree x _ Empty) = x
maximal (BinTree _ _ r)     = maximal r

foldlBin :: (b -> a -> b) -> b -> BinTree a -> b
foldlBin _ identity Empty = identity
foldlBin func identity (BinTree x l r) = foldlBin func (func (foldlBin func identity l) x) r

tree :: BinTree Char
tree = BinTree 'a' (BinTree 'b' (BinTree 'd' (BinTree 'f' (BinTree 'g' Empty Empty) Empty) Empty) (BinTree 'e' Empty Empty)) (BinTree 'c' Empty (BinTree 'h' Empty Empty))

tree2 :: BinTree Int
tree2 = BinTree 1 (BinTree 2 (BinTree 4 (BinTree 6 (BinTree 7 Empty Empty) Empty) Empty) (BinTree 5 Empty Empty)) (BinTree 3 Empty (BinTree 8 Empty Empty))

----------- Another binary search tree from 2015 exam ------

data BinTree2 a = Leaf a | Bin a (BinTree2 a) (BinTree2 a) deriving (Show)

instance Functor BinTree2 where
  fmap func (Leaf x) = Leaf (func x)
  fmap func (Bin x l r) = Bin (func x) (fmap func l) (fmap func r)

fringe :: BinTree2 a -> [a]
fringe (Leaf x) = [x]
fringe (Bin _ l r) = fringe l ++ fringe r

sameFringe :: Eq a => BinTree2 a -> BinTree2 a -> Bool
sameFringe (Leaf x) (Leaf y) = x == y
sameFringe (Leaf _) Bin{} = False
sameFringe Bin{} (Leaf _) = False
sameFringe (Bin _ l1 r1) (Bin _ l2 r2) = sameFringe l1 l2 && sameFringe r1 r2

btMap :: (a -> b) -> BinTree2 a -> BinTree2 b
btMap = fmap

-------------------- Stack -----------------------------

type Stack a = [a]

empty :: [a]
empty = []

push :: a -> Stack a -> Stack a
push x xs = x : xs

pop :: Stack a -> a
pop [] = error "Cannot pop from an empty stack"
pop (x:_) = x

------------------ a function to compress --------------

compress :: Eq a => [a] -> [a]
compress list = compress' list []
  where compress' [] result = result
        compress' [x] result = result ++ [x]
        compress' (x : xs@(y : _)) result
          | x == y    = compress' xs result
          | otherwise = compress' xs (result ++ [x])
