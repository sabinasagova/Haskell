import Data.Set (toList, fromList)

data Prop = 
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

type Assignment = [(Char, Bool)]
type Assign k v  = [(k,v)]

search :: Eq k => k -> Assign k v -> v
search k t = head [v | (k',v) <- t, k == k']

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

eval :: Assignment -> Prop -> Bool
eval _ (Const b) = b
eval a (Var c) = search c a
eval a (Not p) = not (eval a p)
eval a (And p q) = eval a p && eval a q
eval a (Or p q) = eval a p || eval a q

elements :: [Char] -> [Char]
elements = toList . fromList

value :: String -> [Assignment]
value [] = [[]]
value (c:cs) = [a:as | a <- [(c, b) | b <- [True, False]], as <- value cs]

isTaut :: Prop -> Bool
isTaut p = and [eval a p | a <- value (elements (vars p))]