import Data.List
-- $ operator applies a function to a value
-- it has extremely low precedence
-- length ("abc" ++ "def" "ghi")
-- length $ abc ++ def ++ ghi
-- it's like a left parenthesis that you don't need to close

-- write our own type classes
-- IntSet type class: a set of integers
class IntSet where
  empty :: s
  contains :: Int -> s -> Bool
  add :: Int -> s -> s -- add if not already present
  remove :: Int -> s -> s
  
  singleton :: Int -> s
  singleton i =  add i empty
  
data IntList = IntList [Int]

instance IntSet IntList where
    empty = IntList []
    contains i (IntList xs) = elem i xs
    add i (IntList xs)
      | elem i xs = IntList xs
      | otherwise = IntList (i : xs)
    remove i (IntList xs) = IntList (delete i xs)
    
data IntTree = Nil | Node IntTree Int IntTree

instance IntSet IntTree where
    empty = Nil
    contains i Nil = False
    contains i (Node left v right) 
        | i < v = contains i left
        | i > v = contains i right
        | i == v = True
    add = undefined
    remove = undefined
 
 -- makeSet takes a list of integers, puts them into a new set, returns it
 makeSet :: IntSet s => [Int] -> s
 -- makeSet [] = empty
 -- makeSet (x : xs) = add x (makeSet xs)
 -- fold (function) (starting acc) (list)
 makeSet xs = foldr add empty xs
 
 addAll :: IntSet s => [Int] -> s -> s
 addAll = undefined
 
 -- Set type class: a set of values of type a
 class Set s where
    empty :: s a
    contains :: Ord a => a -> s a -> Bool
    add :: Ord a => a -> s a -> s a
    remove:: Ord a => a -> s a -> s a
    
 data Tree a = Nil | Node (Tree a) a (Tree a)
 
 instance Set Tree where
    empty = Nil
    contains i Nil = False
    contains i (Node left v right) 
        | i < v = contains i left
        | i > v = contains i right
        | i == v = True
    add = undefined
    remove = undefined
    
instance Set [] where
    empty = []
    contains = elem
    add i xs
      | elem i xs = xs
      | otherwise = i : xs
    remove = delete
    
class Functor where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- replace k f replaces every value in f with k
-- replace 4 [3, 5, 8] -> [4, 4, 4]
replace :: Functor f => a -> f a -> f a
replace x f = fmap (\_ -> x) f

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Node left v right) =
        Node (fmap f left) (f v) (fmap f right)

-- <$> infix synonym for fmap
-- <$ is the replace function
-- $> is the same function in other order

-- do - generalized list comprehensions
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(a, b) | a <- xs, b <- x] -- arrows backwards

pairs xs ys = do
    a <- xs
    b <- ys
    return (a, b)
    
cz_colors :: [(String, String)]
cz_colors = [("red", "cerveny"), ("blue", "modry")]

fr_colors :: [(String, String)]
fr_colors = [("red", "rouge"), ("blue", "bleu")]

fr_to_cz :: String -> Maybe String
fr_to_cz color = do
    e <- lookup color (map swap fr_colors)
    f <- lookup e cz_colors
    return f

 
 
 
 
 
 
 
