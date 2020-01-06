length' :: [a] -> Int
length' [] = 0
-- length' (a:[]) = 1
length' (x:xs) = 1 + length' xs


reverse' :: [a] -> [a]

reverse' [] = []
-- reverse' (a:[]) = [a]
-- reverse' (a:b:[]) = [b,a]
reverse' (a:as) = reverse' as ++ [a]

zip' :: [a] -> [b] -> [(a,b)]

-- zip' [] [] = []
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

prime_to :: Int -> [Int]
prime_to n = sieve [2..n]
  where sieve [] = []
        sieve (x:xs) = x : sieve [y | y <- xs, (y `rem` x) /= 0]


-- Helper
delete x xs = [a | a <- xs, a /= x]

permute :: (Eq a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (delete x xs)]

-- Home work : -- hanoi
hanoi :: Int -> [(Int, Int)]
hanoi x = (hanoi' x 1 2 3)
hanoi' :: Int -> Int -> Int -> Int -> [(Int, Int)]
hanoi' 0 _ _ _ = []
hanoi' n beg temp dest = top_to_temp ++ bottom_to_dest : top_to_dest
  where top_to_temp = hanoi' (n-1) beg dest temp
        bottom_to_dest = (beg, dest)
        top_to_dest = hanoi' (n-1) temp beg dest

-- twice :: (a -> a) -> a -> a
-- twice f x = (f . f) x

-- thrice :: (a -> a) -> a -> a
-- thrice f x = (f . f . f) x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map' f xs = [(f x) | x <- xs]


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
 | f x = x : filter' f xs
 | otherwise = filter' f xs


