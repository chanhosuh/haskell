factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

perfects :: Int -> [Int]
perfects n = [x| x <- [1..n], sum (factors x) == 2 * x]
-- perfects 500 = [6,28,496]
