{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

import Data.Reflection

import Data.Proxy

import GHC.TypeLits

data Options
    = FirstParent
    | CherryPick

class ToArg (a :: Options) where
    toArg :: Proxy a -> String

instance ToArg FirstParent where
    toArg _ = "--first-parent"

instance ToArg CherryPick where
    toArg _ = "--cherry-pick"

fromArg :: String -> (forall a. ToArg a => Proxy a -> r) -> r
fromArg "--cherry-pick" cont = cont (Proxy :: Proxy CherryPick)
fromArg "--first-parent" cont = cont (Proxy :: Proxy FirstParent)

fromArgs :: [String] -> (forall a. ToArgs a => Proxy a -> r) -> r
fromArgs []     cont = cont (Proxy :: Proxy '[])
fromArgs ("--cherry-pick":xs) cont = fromArgs xs $ \(Proxy :: Proxy r) -> cont (Proxy :: Proxy (CherryPick ': r))
fromArgs ("--first-parent":xs) cont = fromArgs xs $ \(Proxy :: Proxy r) -> cont (Proxy :: Proxy (FirstParent ': r))

data family Files (a :: Options)
data instance Files FirstParent = None
data instance Files CherryPick = Some String

class ToArgs (a :: [Options]) where
    toArgs :: Proxy a -> [String]

instance ToArgs '[] where
    toArgs _ = []

instance forall x xs. (ToArg x, ToArgs xs) => ToArgs (x ': xs) where
    toArgs _ = toArg (Proxy :: Proxy x) : toArgs (Proxy :: Proxy xs)

data Calls
    = Normal
    | WithFiles

newtype OptionsS = O String
    deriving (Show)

instance Reifies Options OptionsS where
    reflect p = undefined

type family Match (a :: Symbol) :: Options where
    Match "--first-parent" = FirstParent
    Match "--cherry-pick" = CherryPick

--f :: forall s. (Reifies s [Char], ToArg s) => Proxy Symbol -> IO ()
--f _ = putStrLn $ toArg (Proxy :: Proxy (Match s))
