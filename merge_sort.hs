merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

merge_sort :: Ord a => [a] -> [a]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort xs = merge sorted_first_half sorted_second_half
    where
        sorted_first_half = merge_sort first_half
        sorted_second_half = merge_sort second_half
        (first_half, second_half) = halve xs

