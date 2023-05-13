leadingZeroes :: (Eq a, Num a) => [a] -> [a]
leadingZeroes [] = []
leadingZeroes z = if last z /= 0
             then z
             else leadingZeroes $ init z
             
printPoly :: (Eq a, Num a,Show a) => [a] -> Int -> String
printPoly [] 0 = show 0
printPoly [s] v = 
    (if (abs s == 1) && (v /= 0 ) then 
        (if  abs s /= s then "-" 
        else "") 
    else show s) 
    ++ case v of                  
        0 -> ""                                             
        1 -> "x"
        u -> "x^" ++ show u
printPoly (x:xs) v  | x == 0 = 
                        printPoly xs (v+1)
                    | otherwise = 
                        printPoly xs (v+1) ++
                        (if abs x == x && x /= 0 then " + " else " - ")
                        ++ (if (abs x == 1) && (v /= 0) then "" else show (abs x))
                        ++ case v of
                            0 -> ""
                            1 -> "x"
                            u -> "x^" ++ show u

addPoly :: (Eq a, Num a,Show a)  => [a] -> [a] -> [a]
addPoly p1 p2 = if length p1 >= length p2
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1

multiplyPoly ::  (Eq a, Num a,Show a) => [a] -> [a] -> [a]
multiplyPoly [] _ = []
multiplyPoly xs ys = if all (==0) xs
                then [0]
                else addPoly (map (*head xs) ys) (0 : multiplyPoly (tail xs) ys)

data Poly a  = Poly [a]
    deriving(Eq)

poly :: (Eq a, Num a) => [a] -> Poly a
poly [] = Poly [0]
poly l = Poly polynomial
    where polynomial = leadingZeroes (reverse l)

instance (Eq a, Num a, Show a) => Show (Poly a) where
    show (Poly l) = printPoly l 0

instance (Eq a, Num a, Show a) => Num (Poly a) where
    (+) (Poly l) (Poly m) = poly (reverse (addPoly l m))
    (*) (Poly l) (Poly m) = poly (reverse (multiplyPoly l m))
    abs (Poly l) = poly (reverse l)
    signum (Poly l) = 1
    fromInteger i = poly [fromInteger i]
    negate (Poly l) = poly (reverse (map ((-1)*) l))





