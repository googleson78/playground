{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Reflection

import Data.Proxy

data Options
    = FirstParent
    | CherryPick

class ToArg (a :: Options) where
    toArg :: Proxy a -> String

instance ToArg FirstParent where
    toArg _ = "--first-parent"

instance ToArg CherryPick where
    toArg _ = "--cherry-pick"

class ToArgs (a :: [Options]) where
    toArgs :: Proxy a -> [String]

instance ToArgs '[] where
    toArgs _ = []

instance forall x xs. (ToArg x, ToArgs xs) => ToArgs (x ': xs) where
    toArgs _ = toArg (Proxy :: Proxy x) : toArgs (Proxy :: Proxy xs)

data Calls
    = Normal
    | WithFiles

newtype OptionsS = O [String]
    deriving (Show)

instance Reifies '[FirstParent] OptionsS where
    reflect p = O $ toArgs (Proxy :: Proxy '[FirstParent])
