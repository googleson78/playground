{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.TypeLits (Nat, type (+), type (<=), KnownNat, natVal)
import GHC.Exts (IsList(fromList, type Item))
import qualified GHC.Exts as L (toList)
import Data.Proxy (Proxy (..))

import Data.List (intercalate)
import Data.Foldable (toList)

import Unsafe.Coerce

import Data.Constraint ((\\), withDict)
import Data.Constraint.Nat (plusIsCancellative, plusAssociates)

-- primitive recursive functions with no compile-time guarantees
-- for argument counts
data PRFun
    = Zero
    | Succ
    | Proj Int
    | Comp PRFun [PRFun]
    | Rec PRFun PRFun
    deriving (Eq, Show)

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

-- primitive recursive functions with type info about
-- argument count, allowing compile-time guarantee for argument counts
data PRFunC (n :: Nat) where
    ZeroC :: forall n. PRFunC n
    SuccC :: PRFunC 1
    ProjC :: (KnownNat m, KnownNat n, m <= n) => Proxy m -> PRFunC (1 + n)
    CompC :: (KnownNat n, KnownNat m) => PRFunC n -> Vector n (PRFunC m) -> PRFunC m
    RecC  :: (KnownNat n) => PRFunC n -> PRFunC (1 + (1 + n)) -> PRFunC (1 + n)

-- we have inexhaustive pattern matches here, because the compiler can't figure out that the unmatched cases are not well-typed
interpC :: forall n. PRFunC n -> Vector n Integer -> Integer
interpC ZeroC _             = 0
interpC SuccC (x ::: Nil)   = succ x
interpC (ProjC m) v         = v !!! m
interpC (CompC f gs) v      = interpC f $ fmap (`interpC` v) gs
interpC (RecC (f :: PRFunC n1) _) (x:::(xs :: Vector n2 Integer))
    | x == 0 = interpC f xs \\ plusIsCancellative @1 @n1 @n2
interpC h@(RecC _ g) (x:::xs)
    = interpC g $ ((x - 1) ::: xs) `snoc` (interpC h $ (x - 1) ::: xs)

const'C :: forall n. (KnownNat n) => PRFunC (1 + n)
const'C = ProjC (Proxy @0)

plusC :: PRFunC 2
plusC = RecC f g
       where f = const'C
             g = CompC SuccC (single $ ProjC $ Proxy @2)

multC :: PRFunC 2
multC = RecC f g
       where f = ZeroC
             g = CompC plusC (ProjC (Proxy @1) ::: ProjC (Proxy @2) ::: Nil)

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
    fmap _ Nil        = Nil
    fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vector n) where
    foldr _ v Nil        = v
    foldr f v (x ::: xs) = f x $ foldr f v xs

snoc :: Vector n a -> a -> Vector (1 + n) a
snoc Nil y = single y
snoc (x:::xs) y = x ::: snoc xs y

(!!!) :: (KnownNat n, KnownNat m, m <= n) => Vector (1 + n) a -> Proxy m -> a
xs !!! p = toList xs !! fromInteger (natVal p)

(<!>) :: forall n m a. Vector n a -> Vector m a -> Vector (n + m) a
Nil                       <!> ys = ys
(x:::(xs :: Vector n1 a)) <!> ys = withDict (plusAssociates @1 @n1 @m) $ x ::: (xs <!> ys)

single :: a -> Vector 1 a
single = (:::Nil)
