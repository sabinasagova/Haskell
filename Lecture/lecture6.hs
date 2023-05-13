my_add :: Int -> Int -> Int
my_add x y = x + y

vec_len :: Float -> Float -> Float
vec_len x y = sqrt (x^2 + y^2)

-- if else
-- fact :: Int -> Int
-- fact x = if x == 0 then 1 else x * fact (x - 1)
-- pattern matching
-- fact 0 = 1
-- fact x = x * fact (x - 1)
-- guards 
-- fact x | x == 0  = 1 -- | means mathematically such that
-- fact x = x * fact (x - 1)

my_gcd :: Int -> Int -> Int
my_gcd a 0 = a
my_gcd a b = my_gcd b (mod a b)

-- ++ appends two lists [2,4] ++ [6,8] makes [2,4,6,8] !! means element by index !! 3 gives us 8

my_head :: [a] -> a
my_head [] = error "head of empty list"
my_head (x : xs) = x

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

append :: [a] -> [a] -> [a]
append [] xs = xs
append (x : xs) ys = x : append xs ys

intsFrom :: Int -> [Int]
intsFrom x = x : intsFrom (x + 1)

-- head "watermelon" not 'watermelon'

range :: Int -> Int -> [Int] 
range x y = if x == y then [x] else x : range (x + 1) y

first :: (a, b) -> a
first (x, _) x
