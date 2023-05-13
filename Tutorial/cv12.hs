import Data.Char
data Mozna a = Proste a | Nic deriving (Show)

safeLog x | x <= 0 = Nic
          | otherwise = Proste (log x)

safeSqrt x | x < 0 = Nic
           | otherwise = Proste (sqrt x)

slozeni :: (a -> Mozna b) -> (b -> Mozna c) -> (a -> Mozna c)
slozeni f g x = gg (f x) where
                gg Nic = Nic
                gg (Proste a) = g a

-- (safeLog x) 'potom' (safeSqrt x)
-- (safeLog 0.1) 'potom' safeSqrt == Nic
-- (safeLog 10) 'potom' safeSqrt == Proste (sqrt(lg10))
potom:: Mozna a -> (a -> Mozna b) -> Mozna b
potom Nic _ = Nic 
potom (Proste x) g = g x

msafeLog x | x <= 0 = Nothing
          | otherwise = Just (log x)

msafeSqrt x | x < 0 = Nothing
           | otherwise = Just (sqrt x)

msafeSqrtLog x1 x2 = do
    y <- msafeLog x1 
    z <- msafeLog x2 
    msafeSqrt (y + z)

instance Functor Mozna where
  fmap f Nic = Nic
  fmap f (Proste x) = Proste (f x)
-- $ to iste ako medzera, ale opacne priority
instance Applicative Mozna where
    --pure:: a -> Mozna 
    pure x = Proste x
    --(<*>):: Mozna (a-> b) -> Mozna a -> Mozna b
    Nic <*> _ = Nic 
    (Proste f) <*> x = fmap f x 

plus3 x y z = x + y + z

instance Monad Mozna where 
    --(>>=)::Mozna a -> (a -> Mozna b) -> Mozna b
    (>>=) = potom
    -- return = pure

usporadaneDvojice xs ys = do
    x <- xs 
    y <- ys 
    return x + y

-- fmap funguje jen pro unarni fce, pre binarni nn
zakric = do
    x <- getLine
    let y = map toUpper x
    putStrLn y  