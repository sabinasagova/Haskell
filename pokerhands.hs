import Data.List (group, sort)
import Control.Arrow ((&&&))

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

countOccurences :: Ord a => [a] -> [(Int, a)]
countOccurences = reverseList . sort . map (length &&& head) . group . sort 

rank :: [Int] -> Int
rank a
    | count 0 == 4                  = 6
    | count 0 == 3 && count 1 == 2  = 5
    | count 0 == 3                  = 4
    | count 0 == 2 && count 1 == 2  = 3
    | count 0 == 2                  = 2
    | otherwise                     = 1
    where count = (map fst (countOccurences a) !!)

better :: [Int] -> [Int] -> Bool
better a b 
    | rank a > rank b = True
    | rank b > rank a = False
    | rank a == 1     = sort b > sort a
    | otherwise       = countA 0 > countB 0
    where countA = (map snd (countOccurences a) !!)
          countB = (map snd (countOccurences b) !!)




    