{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language KindSignatures #-}
{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}
{-# Language PolyKinds #-}

{-# Language UndecidableInstances #-}

import Prelude hiding (curry)

import Data.Kind

import GHC.TypeLits (TypeError(..))

import Unsafe.Coerce

import GHC.Exts (IsList(..))

data HList :: [Type] -> Type where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

infixr 5 :::

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

deriving instance (All Show ts) => Show (HList ts)

type family Split (f :: Type) :: ([Type], Type) where
  Split (a -> b) = '(a ': Fst (Split b), Snd (Split b))
  Split a = '( '[], a)

type family Fst (tup :: (k, p)) :: k where
  Fst '(x, _) = x

type family Snd (tup :: (k, p)) :: p where
  Snd '(_, y) = y

curry :: f -> HList (Fst (Split f)) -> Snd (Split f)
curry f Nil        = unsafeCoerce f
curry f (x ::: xs) = unsafeCoerce $ curry (unsafeCoerce f x) (unsafeCoerce xs)


type family Args (f :: Type) :: [Type] where
  Args (a -> b) = a ': Args b
  Args a = '[]

type family Res (f :: Type) :: Type where
  Res (_ -> a) = Res a
  Res a = a

class Function a where
  curry' :: a -> HList (Args a) -> Res a

instance {-# OVERLAPPABLE #-} Function a where
  curry' x Nil = unsafeCoerce x

instance Function b => Function (a -> b) where
  curry' f (x ::: xs) = curry' (f x) xs
