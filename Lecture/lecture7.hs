-- fibsFrom :: Int -> Int -> [Int]
-- fibsFrom i j = i : fibsFrom j (i + j)

allFibs :: [Int]
allFibs = 
  let fibsFrom i j = i : fibsFrom j (i + j)
  in fibsFrom 1 1

sign :: Int -> Int
-- sign 0 = 0
-- sign x = if x < 0 then -1 else +1
sign x 
  | x < 0 = -1
  | x > 0 = +1
  | otherwise = 0
 
only_odd :: [Int] -> [Int]
only_odd [] = []
only_odd (x:xs)
  | odd x = x : rest
  | otherwise = rest
  where rest = only_odd xs

maxx :: a -> a -> a
maxx x y = if x > y then x else y

-- member x xs is true if is in the list (xs)
member :: Eq a => a -> [a] -> Bool
member _ [] = False
member x (y : ys) = if x == y then True else member x ys

maxx :: Ord a => a -> a -> a
maxx x y = if x > y then x else y

-- sort :: Ord a => [a] -> [a]

-- return a sequence of values [x, ... downward, ... y]
-- downTo 10 1 [10, 9, ... 1]
downTo :: Eq a => Enum a => a -> a -> [a] -- without Enum pred would not work, and without Eq == would not work
downTo x y
  | (x == y) = [x]
  | otherwise = x : downTo (pred x) y

-- read a pair of values, each of which is a word
-- example: read_pair "4 10" -> (4, 10)
read_pair :: Read a => String -> (a, a)
read_pair s =
  let [x, y] = words s
  in (read x, read y)
 
summ :: Num a => [a] -> a -- we need Num to ensure a is numeric, otherwise it will not compile
summ [] = 0
summ (x : xs) = x + summ xs
 
gcdd :: Integral a => a -> a -> a
gcdd x 0 = x
gcdd x y = gcd y (mod x y)

-- list comprehensions [expr | var <- list]

-- flatten a list of lists
-- flatten [[1, 2], [3,4], [5, 6, 7]] -> [1, 2, 3, 4, 5, 6, 7]
flatten :: [[a]] -> [a]
-- flatten [] = []
-- flatten (x : xs) = x ++ flatten xs

flatten xs = [x | l <- xs, x <- l] -- you have to put l <- xs before x <- l

append :: [a] -> [a] -> [a]
append a b = [x | l <- [a, b], x <- l]

-- generate a list of all integers (positive and negative)
all_ints :: [Integer]
all_ints = 0 : [x * s | x <- [1 ..], s <- [1, -1]]
-- all_ints = 0 : [y | x <- [1 ..], y <- [x, -x]]



 





