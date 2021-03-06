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
{-# Language MultiParamTypeClasses #-}

{-# Language UndecidableInstances #-}

import Prelude hiding (curry)

import Data.Kind

import GHC.TypeLits (TypeError, ErrorMessage(..))

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

class Function' a (args :: [Type]) (res :: Type) where
  curry'' :: a -> HList args -> res

-- if we don't "catch all" here with the a b instance head
-- we would need to write out explicit types for
-- curry'' True Nil (so `curry'' True Nil :: Bool`, for example)
-- because Haskell doesn't know that you want the a [] a instance specifically
instance (a ~ b) => Function' a '[] b where
  curry'' x Nil = x

instance ( Function' b (Args b) (Res b)
         , args ~ Args (a -> b)
         , res ~ Res (a -> b)
         )
         => Function' (a -> b) args res where
  curry'' f (x ::: xs) = curry'' (f x) xs

class Function'' a (args :: [Type]) (res :: Type) where
  curry''' :: a -> HList args -> res

-- See note on Function'.
-- We also have a guard here because otherwise we would also match
-- things like `curry (&&) Nil`
instance ( a ~ b
         , Guard b (Not (HasArgs b))
         ) => Function'' a '[] b where
  curry''' x Nil = x

instance ( Function'' b (Args b) (Res b)
         , (t ': args) ~ Args (a -> b)
         , res ~ Res (a -> b)
         )
         => Function'' (a -> b) (t ': args) res where
  curry''' f (x ::: xs) = curry''' (f x) xs

type family HasArgs (t :: Type) :: Bool where
  HasArgs (_ -> _) = True
  HasArgs _ = False

type family Guard (b :: k) (a :: Bool) :: Constraint where
  Guard b False = TypeError (Text "You gave (" :<>: ShowType b :<>: Text ") but me no like.")
  Guard _ True = ()

type family Not (a :: Bool) :: Bool where
  Not True = False
  Not False = True

type family Map (t :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map _ '[] = '[]
  Map t (x ': xs) = t x ': Map t xs

-- TODO: add (Map ap args) to the second argument
class Lift ap a (args :: [Type]) res where
  lift :: ap a -> HList args -> ap res

instance (Applicative ap, a ~ res) => Lift ap a '[] res where
  lift x _ = x

instance ( Applicative ap
         , Lift ap b ts res
         , (t ': ts) ~ Map ap (Args (a -> b))
         , res ~ Res (a -> b)
         )
         => Lift ap (a -> b) (t ': ts) res where
  lift f (x ::: xs) = lift (f <*> x) xs
