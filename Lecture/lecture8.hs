-- association list
colors :: [(String, Int)]
colors = [("red", 3), ("pink", 2)]

foo color = (case color of 
  "red" -> 3
  "green" -> 5
  "blue" -> 7
  _ -> 9) + 1
  
-- lookupp "red" colors -> 3
-- lookupp "blue" colors -> 2

lookupp :: Eq a => a -> [(a, b)] -> Maybe b -- maybe b, maybe nothing
lookupp _ [] = Nothing
lookupp x ((k, v) : l) =
  if x == k then Just v
            else lookup x l

maybeToList :: Maybe a -> [a]
-- maybeToList Nothing = []
-- maybeToList (Just x) = [x]
maybeToList m = case m of 
  Nothing -> []
  Just x -> [x]
  
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

factorial :: Int -> Int
factorial n = case n of
  0 -> 1
  x -> x * factorial (x - 1)

-- insert a value into sorted list
insert :: Ord a => a -> [a] -> [a]
insert _ [] = [] -- QUESTION1
insert x l@(y : ys) = 
  if x < y then x : y : l
           else y : insert x ys
           
-- high-order functions = functions that take functions as arguments

-- mapp : apply a function to each element of a list
mapp : (a -> b) -> [a] -> [b] --QUESTION2 why (a -> b) ! FUNCTION ARROW IS RIGHT-ASSOCIATIVE
mapp _ [] = [] -- map any function to empty list, and we get an empty list
mapp f (x : xs) = f x : mapp f xs

-- filterr : choose list elements for which a predicate is true
filterr :: (a -> Bool) -> [a] -> [a] -- QUESTION2 why again
filterr _ [] = []
filterr f (x : xs) =
  if f x then x : filterr f xs
         else filterr f xs

-- filterr f (x : xs)
--  | f x = x : rest
--  | otherwise = rest
--  | where rest = filter f xs

-- anyy : true if the condition is true for any element in the list
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || any f xs

prepend :: a -> [a] -> [a]
prepend x xs = x : xs

-- flip : transform a function of two arguments by flipping the argument order
-- p2 = flip prepend
-- p2 [3, 4, 5] 2 --> [2, 3, 4, 5] -- QUESTION3 I DO NOT UNDERSTAND FLIP 

flip :: (a -> b -> c) -> (b -> a -> c)
flip f =
  let g x y = f y x
  in g
-- flip f x y = flip y x

-- twice: transform a function f of one argument into a function
-- that applies f twice
-- (twice succ 10) -> 12
twice :: (a -> a) -> (a -> a)
twice f = 
  let g x = f (f x)
  in g

-- twice f x = twice f (f x)
-- the power of a function

pow :: (a -> a) -> Int -> (a -> a)
-- pow f n = 
--  let g 0 x = x
--      g k x = f (g (k - 1) x)
--      in g
pow f 0 x = x
pow f n x = f (pow f (n - 1) x)

-- lambda expressions
add :: Int -> Int -> Int
add x y = x + y
add = (\x y -> x + y)
add = (\x -> \y -> x + y)

comp :: (b -> c) -> (a -> b) -> (a -> c)
comp f g x = f (g x)

-- write a function that converts all words in a string to lowercase,
-- and reverses them
-- revWords "one two three" -> 
revWords s = unwords (reverse (words (map toLower s)))
revWords = unwords . reverse . words . map toLower -- point-free style

-- curry: convert an uncurried function to a curried function
curry :: ((a -> b) -> c) -> (a -> b -> c)
curry f x y = f (x y) -- YOU THINK ABOUT THAT



    

