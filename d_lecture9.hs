intsFrom :: Int -> [Int]
intsFrom i = i : intsFrom (i + 1)

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs
