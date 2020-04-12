import Data.Char

let2int :: Char -> Int
let2int c 
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let i = chr (i + ord 'a')
int2upperlet i = chr(i + ord 'A')

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let((let2int c + n) `mod` 26)
  | isUpper c = int2upperlet((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

isLatinLetter :: Char -> Bool
isLatinLetter c = isLower c || isUpper c

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z']]
           where n = letters cs

count :: Char -> String -> Int
count c cs = sum [1 | c' <- cs, toLower c' == c]

letters :: String -> Int
letters cs = sum [1 | c <- cs, isLatinLetter c]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o-e)^2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Float -> [Float] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

crack :: String -> String
crack cs = encode (-factor) cs
    where factor = head (positions (minimum chitab) chitab)
          chitab = [chisqr (rotate n table') table | n <- [0..25]]
          table' = freqs cs

{- Tests
*Main> encode 3 "Haskell is fun"
"Kdvnhoo lv ixq"
*Main> crack "Kdvnhoo lv ixq"
"Haskell is fun"

crack (encode 17 "To be, or not to be, that is the question: Whether 'tis nobler in the mind to suffer The slings and arrows of outrageous fortune, Or to take Arms against a Sea of troubles, And by opposing end them: to die, to sleep; No more; and by a sleep, to say we end The heart-ache, and the thousand natural shocks That Flesh is heir to? 'Tis a consummation Devoutly to be wished. To die, to sleep, perchance to Dream; aye, there's the rub, For in that sleep of death, what dreams may come, When we have shuffled off this mortal coil, Must give us pause." )

-}

