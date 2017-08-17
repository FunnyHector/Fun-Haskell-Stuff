data Tree = Leaf Int
          | BranchL { num :: Int, left :: Tree }
          | BranchR { num :: Int, right :: Tree }
          | BranchLR { num :: Int, left :: Tree, right :: Tree }
            deriving (Show)

sumTree :: Tree -> Int
sumTree (Leaf n) = n
sumTree (BranchL n left) = n + sumTree left
sumTree (BranchR n right) = n + sumTree right
sumTree (BranchLR n left right) = n + sumTree left + sumTree right
