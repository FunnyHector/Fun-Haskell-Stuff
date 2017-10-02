module Test where

table :: [(String, Int)]
table = [("hello", 5), ("world", 10), ("apple", 3), ("orange", 12)]

-- my version of lookup
lookup_ :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup_ _ [] = Nothing
lookup_ t ((k,v):ps)
  | t == k = Just v
  | otherwise = lookup_ t ps

-- Sum of values at keys a and b in Map d
sum2 :: String -> String -> [(String, Int)] -> Maybe Int
sum2 a b d = do x <- lookup a d
                y <- lookup b d
                return (x + y)

-- Sum of values at keys a and b in Map d
sum2' :: String -> String -> [(String, Int)] -> Maybe Int
sum2' a b d = case lookup a d of
                (Just x) -> case lookup b d of
                              (Just y) -> Just (x + y)
                              Nothing  -> Nothing
                Nothing  -> Nothing



-- This succeeds for the key "hello", but calls fail for any other.
badLookup k d = do 5 <- lookup k d
                   return k


badLookup' k d = lookup k d >>= \5 -> return k

-- This is a "safe" version of the head :: [a] -> a function.
getFirst :: [a] -> Maybe a
getFirst (x:xs) = Just x
getFirst _ = Nothing



-- Safe (!!): how should it work?
getAt :: [a] -> Int -> Maybe a
getAt (x:xs) v = if v == 0 then Just x else getAt xs (v - 1)
getAt [] _ = Nothing

firstLessFifth l = do x <- getFirst l
                      y <- getAt l 4
                      return (x - y)
