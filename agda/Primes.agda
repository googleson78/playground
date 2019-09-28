{-# OPTIONS --no-unicode #-}

module Primes where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}

data Zero : Set where

naughty : {X : Set} -> Zero -> X
naughty ()

record One : Set where
  constructor <>

data List (X : Set) : Set where
  [] : List X
  _,-_ : X -> List X -> List X

data _==_ {X : Set} (x : X) : X -> Set where
  refl : x == x

{-# BUILTIN EQUALITY _==_ #-}

infix 2 _==_

ap : {X Y : Set} {x y : X} (f : X -> Y) -> x == y -> f x == f y
ap f refl = refl

data Decide (X : Set) : Set where
  yes : X -> Decide X
  no : (X -> Zero) -> Decide X

eqNat : (n : Nat) -> (m : Nat) -> Decide (n == m)
eqNat zero zero = yes refl
eqNat zero (suc m) = no (\ ())
eqNat (suc n) zero = no (\ ())
eqNat (suc n) (suc m) with eqNat n m
eqNat (suc n) (suc m) | yes n==m = yes (ap suc n==m)
eqNat (suc n) (suc m) | no n==m = no (\ { refl -> n==m refl})

record Sg (X : Set) (P : X -> Set) : Set where
  constructor _,_
  field
    x : X
    px : P x

_*_ : Set -> Set -> Set
X * Y = Sg X \ _ -> Y

infixr 4 _*_

reverse : {X : Set} -> List X -> List X
reverse xs = help xs []
  where
  help : {X : Set} -> List X -> List X -> List X
  help [] ys = ys
  help (x ,- xs) ys = help xs (x ,- ys)

map : {X Y : Set} -> (X -> Y) -> List X -> List Y
map f [] = []
map f (x ,- xs) = f x ,- map f xs

_+_ : Nat -> Nat -> Nat
zero + m = m
suc n + m = suc (n + m)

+zeroId : (n : Nat) -> n + 0 == n
+zeroId zero = refl
+zeroId (suc n) rewrite +zeroId n = refl

data _<=_ : Nat -> Nat -> Set where
  or : {n : Nat} -> n <= n
  o' : {n m : Nat} -> n <= m -> n <= suc m

oz : (n : Nat) -> zero <= n
oz zero = or
oz (suc n) = o' (oz n)

suc<=zero-impossible : (n : Nat) -> suc n <= zero -> Zero
suc<=zero-impossible n ()

os : (n m : Nat) -> n <= m -> suc n <= suc m
os n .n or = or
os n (suc m) (o' p) = o' (os n m p)

op : (n m : Nat) -> suc n <= suc m -> n <= m
op n .n or = or
op zero (suc m) (o' p) = o' (oz m)
op (suc n) (suc m) (o' p) = o' (op (suc n) m p)

leqNat : (n m : Nat) -> Decide (n <= m)
leqNat zero m = yes (oz m)
leqNat (suc n) zero = no (suc<=zero-impossible n)
leqNat (suc n) (suc m) with leqNat n m
leqNat (suc n) (suc m) | yes ineq = yes (os n m ineq)
leqNat (suc n) (suc m) | no ineq = no \ sucineq -> ineq (op n m sucineq)

fromTo' : {n m : Nat} -> n <= m -> List Nat
fromTo' {n} or = n ,- []
fromTo' {m = suc m} (o' p) = suc m ,- fromTo' p

fromTo : Nat -> Nat -> List Nat
fromTo n m with leqNat n m
fromTo n m | yes p = reverse (fromTo' p)
fromTo n m | no _ = []

foldl : {X Y : Set} -> (Y -> X -> Y) -> Y -> List X -> Y
foldl f v [] = v
foldl f v (x ,- xs) = foldl f (f v x) xs

scanl : {X Y : Set} -> (Y -> X -> Y) -> Y -> List X -> List Y
scanl f v [] =  v ,- []
scanl f v (x ,- xs) = v ,- scanl f (f v x) xs

replicate : {X : Set} -> Nat -> X -> List X
replicate zero x = []
replicate (suc n) x = x ,- replicate n x

_-_ : Nat -> Nat -> Nat
zero - m = zero
suc n - zero = suc n
suc n - suc m = n - m

fromTo'' : (n m : Nat) -> List Nat
fromTo'' n m = map (n +_) (scanl _+_ 0 (replicate (m - n) 1))

All : {X : Set} (P : X -> Set) -> List X -> Set
All P [] = One
All P (x ,- xs) = P x * All P xs

-- good luck
leqMax : (n m : Nat) -> All (\ k -> n <= k * k <= m) (fromTo'' n m)
leqMax zero zero = (or , or) , <>
leqMax zero (suc m) = (or , (o' (oz m))) , {!!}
leqMax (suc n) zero = {!!}
leqMax (suc n) (suc m) = {!!}
