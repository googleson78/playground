calc :: String -> Int
calc = fst . calc' . words

calc' :: [String] -> (Int, [String])
calc' ("+":xs) = (first + second, restS)
    where (first, restF)  = calc' xs
          (second, restS) = calc' restF

calc' ("*":xs) = (first * second, restS)
    where (first, restF)  = calc' xs
          (second, restS) = calc' restF

calc' (x:xs) = (numx, xs)
    where numx = read x


data Expr = Num Int
          | Sum Expr Expr
          | Mult Expr Expr
            deriving (Show, Eq)
