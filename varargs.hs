class SumArgs a where
    sappend :: [Integer] -> a

instance (Integral a, SumArgs r) => SumArgs (a -> r) where
    --sappend acc = \x -> sappend (toInteger x : acc)
    sappend acc = sappend . (:acc) . toInteger

instance SumArgs Integer where
    sappend = sum

plus :: (SumArgs a) => a
plus = sappend []
