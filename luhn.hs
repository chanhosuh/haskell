luhn :: Int -> Int -> Int -> Int -> Bool

luhnDouble :: Int -> Int

luhnDouble n = (2 * n) `mod` 9

luhn x y z w = sum [luhnDouble x, y, luhnDouble z, w] `mod` 10 == 0

{-
 test cases
 
1 7 8 4     True
4 7 8 3     False

 -}
