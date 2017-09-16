module Test where

data Two = L Int | R Bool deriving (Show)

getTwo :: Int -> Two
getTwo x = if x > 5 then L x else R False


testFun :: Int -> String
testFun x = if True then show b else show c
            where (R b) = getTwo x
                  (L c) = getTwo x
