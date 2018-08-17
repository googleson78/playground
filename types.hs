{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

data Prints where
    Prints :: forall a. Show a => a -> Prints

instance Show Prints where
    show (Prints x) = show x

-- ??????
data PrintsList where
    Nil :: PrintsList
    Cons :: forall a. Show a => a -> PrintsList -> PrintsList
