class SumResult a where
    sappend :: [Integer] -> a

instance (Integral a, SumResult r) => SumResult (a -> r) where
    --sappend acc = \x -> sappend (toInteger x : acc)
    sappend acc = sappend . (:acc) . toInteger

instance SumResult Integer where
    sappend = sum

plus :: (SumResult a) => a
plus = sappend []
