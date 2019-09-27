{-# OPTIONS --no-unicode #-}

module Reclang where

data Zero : Set where

record One : Set where
  constructor <>

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}

_+_ : Nat -> Nat -> Nat
zero + m = m
suc n + m = suc (n + m)

_<=_ : Nat -> Nat -> Set
zero <= m = One
suc n <= zero = Zero
suc n <= suc m = n <= m

data Vector (X : Set) : Nat -> Set where
  nil : Vector X 0
  _,-_ : {n : Nat} -> X -> Vector X n -> Vector X (suc n)

infixr 5 _,-_

ix : {X : Set} {n : Nat} -> (m : Nat) -> suc m <= n -> Vector X n -> X
ix zero <> (x ,- xs) = x
ix (suc m) p (x ,- xs) = ix m p xs

vector : {X Y : Set} {n : Nat} -> (X -> Y) -> Vector X n -> Vector Y n
vector f nil = nil
vector f (x ,- xs) = f x ,- vector f xs

pure : {X : Set} {n : Nat} -> X -> Vector X n
pure {n = zero} x = nil
pure {n = suc n} x = x ,- pure x

ap : {X Y : Set} {n : Nat} -> Vector (X -> Y) n -> Vector X n -> Vector Y n
ap nil nil = nil
ap (f ,- fs) (x ,- xs) = f x ,- ap fs xs

data PrimitiveRec : Nat -> Set where
  zero : PrimitiveRec 0
  suc : PrimitiveRec 1
  proj : {m : Nat} -> (n : Nat) -> {_ : suc n <= m} -> PrimitiveRec m
  comp : {n m : Nat} -> PrimitiveRec n -> Vector (PrimitiveRec m) n -> PrimitiveRec m
  rec : {n : Nat} -> PrimitiveRec n -> PrimitiveRec (2 + n) -> PrimitiveRec (suc n)

interp : {n : Nat} -> PrimitiveRec n -> Vector Nat n -> Nat
interp zero nil = zero
interp suc (x ,- nil) = suc x
interp (proj {m} n {p}) = ix n p
interp (comp {n} {m} f fs) xs =  interp f (ap (helpMapInterp fs) (pure xs))
  where -- termination checker - conor talked about this issue
  helpMapInterp : {n : Nat} -> Vector (PrimitiveRec m) n -> Vector (Vector Nat m -> Nat) n
  helpMapInterp nil = nil
  helpMapInterp (x ,- xs) = interp x ,- helpMapInterp xs
interp (rec f g) (zero ,- xs) = interp f xs
interp (rec f g) (suc x ,- xs) = interp g (x ,- (interp (rec f g) (x ,- xs)) ,- xs)

add : PrimitiveRec 2
add = rec (proj 0) (comp suc (proj 1 ,- nil))
