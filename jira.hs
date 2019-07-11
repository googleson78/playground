{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}
{-# Language InstanceSigs #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language BlockArguments #-}
{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}

import Data.Kind (Type, Constraint)
import Data.Aeson
import GHC.TypeLits (Symbol)

import qualified Data.HashMap.Lazy as M

data HList (ts :: [Type]) where
  Nil   :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

instance Show (HList '[]) where
  show Nil = "Nil"

deriving instance (Show t, Show (HList ts)) => Show (HList (t ': ts))

infixr 6 :::

data FieldI
  = SummaryT
  | StatusT

data family Field (t :: FieldI)

data instance Field SummaryT = Summary String
data instance Field StatusT = Status String

class Objectively a where
  objectify :: a -> Object

class Subjectively a where
  subjectify :: Object -> a

instance Subjectively (HList '[]) where
  subjectify _ = Nil

instance ( Subjectively t
         , Subjectively (HList ts)
         )
        => Subjectively (HList (t ': ts)) where
  subjectify o = subjectify o ::: subjectify o

type family All (p :: k -> Constraint) (ts :: [k]) :: Constraint where
  All p '[] = ()
  All p (t ': ts) = (p t, All p ts)

type family Map (f :: k -> Type) (ts :: [k]) :: [Type] where
  Map _ '[] = '[]
  Map f (t ': ts) = f t ': Map f ts

newtype Issue (fields :: [FieldI])
  = Issue {getIssue :: HList (Map Field fields)}
  deriving newtype (ToJSON)

instance (All Objectively ts, All ToJSON ts) => ToJSON (HList ts) where
  toJSON Nil = Object M.empty
  toJSON (x ::: xs) = Object $ objectify x <> fromObject (toJSON xs)
    where
      fromObject :: Value -> Object
      fromObject (Object o) = o
      fromObject _ = error "wtf man"
      {-# INLINE fromObject #-}

instance (Subjectively (HList (Map Field ts))) => FromJSON (Issue ts) where
  parseJSON = withObject "Issue" (pure . Issue . subjectify)
