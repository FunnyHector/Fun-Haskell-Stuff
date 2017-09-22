> module Map where

Author: Lindsay Groves, VUW, 2017.

Implement a map as an unordered list of key-value pairs with no duplicate
keys.  These are all done directly - could instead implement in terms of set
operations.

Operations provided:
emptyMap :: Map a b
hasKey :: a -> Map a b -> Bool
setVal :: a -> b -> Map a b -> Map a b
getVal :: a -> Map a b -> b
delKey :: a -> Map a b -> Map a b

> type Map a b = [(a, b)]

> emptyMap :: Map a b
> emptyMap = []

> hasKey :: Eq a => a -> Map a b -> Bool
> hasKey k = any (\(x,_) -> x == k)

> setVal :: Eq a => a -> b -> Map a b -> Map a b
> setVal k v m = (k,v) : delKey k m

> getVal :: Eq a => a -> Map a b -> b
> getVal k m
>  | null r    = error "Missing key"
>  | otherwise = snd $ head r
>  where r = dropWhile (\(x,_) -> x /= k) m

> delKey :: Eq a => a -> Map a b -> Map a b
> delKey k = filter (\(x,_) -> x /= k)

equals:
Checks whether two maps have same pairs.
(In many languages triple equals means identity equivalence. Here it's not.
This function provides convenience for testing)

> (===) :: (Eq a, Eq b) => Map a b -> Map a b -> Bool
> (===) mapa mapb
>   = isSubMap mapa mapb && isSubMap mapb mapa
>     where isSubMap a b = all (`elem` b) a


merge:
merge the second map into the first map. A pair with new key will be added, and
a pair with existing key will overwrite the old pair.

> merge :: Eq a => Map a b -> Map a b -> Map a b
> merge mapa []               = mapa
> merge mapa ((k, v) : pairs) = merge (setVal k v mapa) pairs

delMap:
remove any pair whose key occurs in the second map.

> delMap :: Eq a => Map a b -> Map a b -> Map a b
> delMap mapa []               = mapa
> delMap mapa ((k, _) : pairs) = delMap (delKey k mapa) pairs

keys:
get all the keys

> keys :: Eq a => Map a b -> [a]
> keys = map fst
