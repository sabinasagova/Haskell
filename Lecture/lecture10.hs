import Data.Array

-- data constructor with no arguments
data Color = Red | Blue | Green

-- recursive type
data Tree a = Nil | Node { left :: (Tree a), val :: a , right :: (Tree a)}

root :: Tree a -> a
root (Node _ val _) = val
root Nil = error "empty"

-- a person has a first name, a last name, and an age
data Person = Person { fistName :: String, lastName :: String, Age :: Int } deriving (Show)

firstAndLast :: Person -> String
firstAndLast (Person f l _) = f ++ l

nums :: [Int]
nums = [2, (a ! 0) + 1, (a ! 1) + 2]

a :: Array Int Int
a = listArray (0, 2) nums

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- top-down dynamic programming = memoization
fib1 :: Int -> Int
fib1 n = 
    -- a[n] is the nth Fibonacci number
    let a = listArray (1, n) (1 : 1 : [a ! (i - 1) + a ! (i - 2) | i <- [3 .. n]]) 
    in a ! n

fib2 :: Int -> Int
fib2 n =
    let a = listArray (1, n) (map f [1 .. n])
        f 1 = 1 
        f 2 = 1
        f k = a ! (k - 1) + a ! (k - 2)
     in a ! n

-- we want to invent a fractional type, i.e. rational numbers
data Frac = Frac Int Int

instance Eq Frac where
  -- Frac a b == Frac c d = (a * d == b * c)
  (==) (Frac a b) (Frac c d) = (a * d == b * c)
  (/=) (Frac a b) (Frac c d) = (a * d /= b * c)
  
instance Ord Frac where
  (<=) (Frac a b) (Frac c d) = (a * d <= b * c)

instance Show Frac where
  show (Frac a b) = show a ++ "/" ++ show b

 


