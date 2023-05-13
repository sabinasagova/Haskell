import  Data.Char 

capitalize :: String -> String
capitalize s = zipWith upper (' ':s) s where
    upper c1 c2 | isSpace c1 && isLower c2 = toUpper c2
    upper c1 c2 = c2

join :: String -> [String] -> String
join separator [] = []
join separator [x] = x
join separator (x:xs) = x ++ separator ++ join separator xs

strWords :: String -> [String]
strWords [] = []
strWords (' ':t) = strWords t
strWords [x] = [[x]]
strWords (x:' ':z) = [[x]] ++ strWords z
strWords (x:y:z) = (x:head rest) : tail rest
    where
        rest = strWords (y:z)