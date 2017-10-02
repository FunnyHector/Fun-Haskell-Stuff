module Test where

-- not :: Bool -> Bool --- flips True <-> False
swap :: (Functor f) => f Bool -> f Bool
swap = fmap not

-- Tree definition from slides
data Tree a = Leaf a | Parent (Tree a) (Tree a)
    deriving (Show)
instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Parent l r) = Parent (fmap f l) (fmap f r)

-- Sample trees to save typing
boolTree = Parent (Leaf True) (Parent (Leaf False) (Parent (Leaf True) (Leaf False)))
numTree = Parent (Parent (Leaf 5) (Leaf 12)) (Parent (Leaf 3) (Leaf 9))
strTree = Parent (Parent (Leaf "hello")
                         (Parent (Leaf "x")
                                 (Leaf "Hi")))
                 (Leaf "COMP304")

-- Composing functions together is the same as fmap (10*) $ fmap length
tenLengths :: (Functor f) => f String -> f Int
tenLengths = fmap ((*10) . length)




-- printTree uses fmap to turn the values into strings
printTree :: (Show a) => Tree a -> IO ()
printTree t = printTree' (fmap show t) ""

-- printTree' is a standard recursive IO function
printTree' (Leaf s) prefix = putStrLn (prefix ++ "-" ++ s)
printTree' (Parent l r) prefix = do putStrLn (prefix ++ "\\")
                                    printTree' l (prefix ++ " |")
                                    printTree' r (prefix ++ " |")





-- From lecture 14:
table :: [(String, Int)]
table = [("hello", 5), ("world", 10), ("apple", 3), ("orange", 12)]

-- lookup :: (Eq a) => a -> [(a, b)] -> Maybe b

-- Sum of values at keys a and b in Map d
sum2 :: String -> String -> [(String, Int)] -> Maybe Int
sum2 a b d = do x <- lookup a d
                y <- lookup b d
                return (x + y)

-- Using applicative functor operations instead:
sum2' a b d = (+) <$> lookup a d <*> lookup b d

-- Adding a third case becomes more complicated (does it?)
sum3' a b c d = (\x y z -> x + y + z) <$> lookup a d <*> lookup b d
                                      <*> lookup c d

-- Implementation of the "ap" function
myAp :: (Monad m) => m (a -> b) -> m a -> m b
myAp m1 m2 = do x1 <- m1
                x2 <- m2
                return (x1 x2)
