
import System.Random
data Tree a = Nil | Node (Tree a) a (Tree a)

instacne Foldable Tree where
    foldr f a Nil = a -- there is nothing to apply
    foldr f a (Node left v right) = 
      foldr f (f v (foldr f a right)) left

-- I/O
-- putStrLn
fac :: Int -> Int
fac 0 = do
  putStrLn "in base case"
  return 1
fac n = n * fac (n - 1)


loop :: Int -> Int -> IO [Int]
loop i n =
  if i > n then return []
  else do
      putStrLn $ "enter number " ++ show i ++ "? "
      k <- readLn
      ks <- loop (i + 1) n
      return $ k : ks
      
run :: Int -> IO ()
run n = do
    nums <- loop 1 to n
    putStrLn $ "The numbers are " ++ show nums
 
get :: Int -> IO Int
get i = do
    putStrLn $ "enter number " ++ show i ++ "? "
    k <- readLn
    return k

guess :: Int -> IO ()
guess x = do
    putStrLn "Your guess?"
    i <- readLn
    if i < x then do
      putStrLn "Too low!"
      guess x
    else if i > x then do
      putStrLn "Too high!"
      guess x
    else 
      putStrLn "Right!"

game :: Int -> IO ()
game x = do
  putStrLn "I'm thinking of a number"
  guess x
  
main :: IO ()
main = do
  gen <- newStdGen
  let (n, _) = randomR (1, 1000) gen 
  game n

 

    
