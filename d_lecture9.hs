intsFrom :: Int -> [Int]
intsFrom i = i : intsFrom (i + 1)

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

union_all :: Eq a => [[a]] -> [a]
union_all [] = []
union_all (l : ll) = union l (union_all ll)

-- foldr we need to give it 1. function 2. accumulator 3.list
sum :: Num a => [a] -> a
sum l = foldr (+) 0 l
sum l = foldl (+) 0 l -- addition is associative

length :: [a] -> Int
length l = foldr (\_ a -> a + 1) 0 l -- take element _, acc and return a + 1

union_all :: Eq a => [[a]] -> [a]
union_all = foldr union [] l

-- foldr (:) [2,4] [6,8] will produce [6,8,2,4]
foldr :: (b -> a -> a) -> a -> [b] -> a
foldr f a [] = a
foldr f a (x : xs) = f x (foldr f a xs)

rev :: [a] -> [a]
rev l =
  let f [] a = a
      f (x : xs) a = f xs (x : a)
      in f l []
rev l = foldl (\a b -> b : a) [] l -- function flips the order 

-- foldl acc always on the left

-- dup: duplicates every element in a list
dup [] = []
dup (x : xs) = x : x : dup xs 

type String 
type Pos = (Float, Float)

-- polymorphic type
type Pair t = (t,t)

swap :: Pair t -> Pair t
swap (x,y) = (y,x)

-- association list of key-value pairs
type Assoc k v = [(k,v)]

lookup :: Eq k => (Assoc k v) -> k -> Maybe v

data Color = Red | Green | Blue deriving (Show) -- shows colors 

code :: Color -> Int
code Red = 1
code Green = 2
code Blue = 3

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle w h) = w * h

data Bool = True | False
  deriving (Show)
  
data Maybe a = Just a | Nothing

data Tree a = Nil | Node (Tree a) a (Tree a)

size :: Tree a -> Int
size Nil = 0
size (Node left _ right) = size left + 1 + size right

flatten :: Tree a -> [a]
flatten (Node left v right) = flatten left ++ [v] ++ flatten right











