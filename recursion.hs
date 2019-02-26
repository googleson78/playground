{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.TypeLits (Nat, type (+), type (-), type (<=), KnownNat, natVal)
import GHC.Exts (IsList(fromList, type Item))
import qualified GHC.Exts as L (toList)
import Data.Proxy (Proxy (..))

import Data.List (intercalate)
import Data.Foldable (toList)

import Unsafe.Coerce

import Data.Constraint ((\\), withDict)
import Data.Constraint.Nat (plusIsCancellative, plusAssociates)

data PRFun
    = Zero
    | Succ
    | Proj Int
    | Comp PRFun [PRFun]
    | Rec PRFun PRFun
    deriving (Eq, Show)

data PRFun1 (n :: Nat) where
    Zero1 :: PRFun1 0
    Succ1 :: PRFun1 1
    Proj1 :: (KnownNat m, KnownNat n, m <= n) => Proxy m -> PRFun1 (1 + n)
    Comp1 :: (KnownNat n, KnownNat m) => PRFun1 n -> Vector n (PRFun1 m) -> PRFun1 m
    Rec1  :: (KnownNat n) => PRFun1 n -> PRFun1 (2 + n) -> PRFun1 (1 + n)

interp1 :: forall n. PRFun1 n -> Vector n Integer -> Integer
interp1 Zero1 _             = 0
interp1 Succ1 (x ::: Nil)   = succ x
interp1 (Proj1 m) v         = v !!! m
interp1 (Comp1 f gs) v      = interp1 f $ fmap (`interp1` v) gs
interp1 h@(Rec1 (f :: PRFun1 n1) g) (x:::(xs :: Vector n2 Integer))
    = if x == 0
      then interp1 f xs \\ plusIsCancellative @1 @n1 @n2
      else interp1 g $ ((x - 1) ::: xs) <!> single ((interp1 h $ (x - 1) ::: xs))

interp :: PRFun -> ([Int] -> Int)
interp (Zero) = const 0
interp (Succ) = succ . head
interp (Proj n) = (!! n)
interp (Comp f gs) = (\xs -> let interp_gs = map interp gs
                                 applied_gs = map ($ xs) interp_gs
                             in  interp f $ applied_gs)
interp h@(Rec f g) = (\(x:xs) -> if x == 0
                                   then interp f $ xs
                                   else interp g $ (x - 1):xs ++ [interp h $ (x - 1):xs])

const' :: PRFun
const' = Proj 0

plus :: PRFun
plus = Rec f g
       where f = const'
             g = Comp Succ [Proj 2]

mult :: PRFun
mult = Rec f g
       where f = Zero
             g = Comp plus [Proj 1, Proj 2]

data Vector (n :: Nat) a where
    Nil   :: Vector 0 a
    (:::) :: a -> Vector n a -> Vector (1 + n) a

infixr 3 :::

instance (KnownNat n) => IsList (Vector n a) where
    type Item (Vector n a) = a
    fromList xs
        | length xs == (fromIntegral $ natVal $ Proxy @n) = unsafeCoerce xs
        | otherwise = error "Don't call fromList like an idiot!"
    toList = Data.Foldable.toList

instance (KnownNat n, Show a) => Show (Vector n a) where
    show xs = (show $ natVal $ Proxy @n) ++ " [" ++ (intercalate "," $ toList $ fmap show xs) ++ "]"

instance Functor (Vector n) where
    fmap f Nil        = Nil
    fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vector n) where
    foldr _ v Nil        = v
    foldr f v (x ::: xs) = f x $ foldr f v xs

(!!!) :: (KnownNat n, KnownNat m, m <= n) => Vector (1 + n) a -> Proxy m -> a
xs !!! p = toList xs !! fromInteger (natVal p)

(<!>) :: forall n m a. Vector n a -> Vector m a -> Vector (n + m) a
Nil                       <!> ys = ys
(x:::(xs :: Vector n1 a)) <!> ys = withDict (plusAssociates @1 @n1 @m) $ x ::: (xs <!> ys)

single :: a -> Vector 1 a
single = (:::Nil)
