{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

class Eval d c | d -> c where
    eval :: d -> c

data Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
    eval (Fst (x, _)) = x
    {-# INLINE eval #-}


data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe [])    = Nothing
    eval (ListToMaybe (x:_)) = Just x
    {-# INLINE eval #-}


data Map a b = Map (a -> b) [a]

instance (Eval b c) => Eval (Map a b) [c] where
    eval (Map _ [])     = []
    eval (Map f (x:xs)) = eval (f x) : eval (Map f xs)
    {-# INLINE eval #-}


data Id a = Id a

instance Eval (Id a) a where
    eval (Id x) = x
    {-# INLINE eval #-}
