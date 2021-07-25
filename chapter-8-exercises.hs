data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

balanced :: Tree a -> Bool
num_leaves :: Tree a -> Int

balanced (Leaf _) = True
balanced (Node x y) = balanced x && balanced y && (abs(num_leaves x - num_leaves y) <= 1)

num_leaves (Leaf _) = 1
num_leaves (Node x y) = num_leaves x + num_leaves y

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs


halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where half = length xs `div` 2


data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\x -> 1) (+)

