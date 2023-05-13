main :: IO()
main = do
  content <- readFile "words"
  let wordList = lines content
  putStrLn "I'm thinking of a word:"
  putStrLn (underline (head wordList))
  play wordList

underline :: String -> String
underline = flip replicate '-' . length

play :: [String] -> IO ()
play wordList =
  do
    putStrLn "Your guess?"
    guess <- getLine
    if length guess == length (head wordList)
      then if guess == head wordList
        then do
          putStrLn "Correct!\nPlay again?"
          answer <- getLine
          if answer == "yes"
            then do
              putStrLn "I'm thinking of a word:"
              putStrLn (underline (wordList !! 1))
              play (tail wordList)
            else 
              return ()
        else do
          putStrLn (match (head wordList) guess)
          play wordList
      else do
        putStrLn "Wrong number of letters"
        play wordList

match :: String -> String -> String
match xs ys =
  [if elem x ys then x else '-' | x <- xs]