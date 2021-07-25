import Prelude hiding (all, any, takeWhile, dropWhile, filter, map)
import qualified Prelude as P (filter, map)

-- 1. [f x | x <- xs, p x], express this using map and filter
map_filter f p xs = P.map f (P.filter p xs)
{-
*Main> xs = [1..10]
*Main> map_filter (^2) even xs
[4,16,36,64,100]
-}

-- 2. 
-- a. decide if all elements of a list satisfy a predicate
all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x y -> p x && y) True
-- b. decide if any element of a list satisfies a predicate
any ::  (a-> Bool) -> [a] -> Bool
any p = foldr (\x y -> p x || y) False
-- c. select elements from a list while they satisfy a predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) 
    | p x = (x : takeWhile p xs)
    | otherwise = []

-- d. remove elements from a list while they satisfy a predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
    | p x = dropWhile p xs
    | otherwise = (x:xs)

-- 3.
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x ys -> (f x : ys)) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr op []
    where x `op` ys | p x = (x:ys)
                    | otherwise = ys

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl op 0 
    where x `op` y = 10 * x + y

