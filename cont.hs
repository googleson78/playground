{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

runCont :: Cont a -> a
runCont (Cont f) = f id

instance Functor Cont where
    fmap :: (a -> b) -> Cont a -> Cont b
    fmap f (Cont g) = Cont $ g . (. f)

instance Applicative Cont where
    pure :: a -> Cont a
    pure x = Cont $ ($ x)

    (<*>) :: Cont (a -> b) -> Cont a -> Cont b
    Cont f <*> Cont x = Cont $ ($ f id $ x id)

instance Monad Cont where
    (>>=) :: Cont a -> (a -> Cont b) -> Cont b
    Cont k >>= f = k . (. f) $ id
