{-# LANGUAGE GADTs #-}
-- extension we need - generalized algebraic data types
{-# LANGUAGE StandaloneDeriving #-}
-- we can't actually use normal deriving for GADTs
-- so we also need "standalone" deriving statements

import Prelude hiding (Maybe(..))

-- basic example:
--
-- here's the classic maybe data type
data Maybe a = Nothing | Just a

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- and here's Maybe with GADT syntax

-- we just list all the constructors *as functions* that return something of our type
data MaybeGADT a where
    NothingGADT :: MaybeGADT a
    JustGADT :: a -> MaybeGADT a

-- we can still pattern match as usual
isJustGADT :: MaybeGADT a -> Bool
isJustGADT NothingGADT = False
isJustGADT (JustGADT _) = True

-- now to do finite lists
-- we want to encode the length of a list in its type
-- for that we need type level natural numbers
-- the same way natural numbers are just
-- 1) the number zero
-- 2) the successor function applied to other natural numbers
--
-- so we have

data Zero
data Succ n

-- they have no constructors because we don't actually need values of these types
-- for example we mean Succ Zero to represent 1
type One = Succ Zero
-- and Succ (Succ (Succ (Succ (Succ Zero)))) is 5
type Five = Succ (Succ (Succ (Succ (Succ Zero))))


-- now for our finite lists
-- other than the type which they hold - a, we will also want to have
-- information about their length in the type
-- so its only natural that
-- 1) the empty list (Nil) has length Zero
-- 2) our inductive case (Cons) increases the length by one
-- in other words if we want to cons to a Vector of length n
-- then we will get a Vector of length (Succ n)
data Vector n a where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

-- so we can write infix Cons'es
infixr 5 `Cons`

-- we need to say that the Vector is Eq only if the a's are Eq
deriving instance (Eq a) => Eq (Vector n a)
-- same for Show
deriving instance (Show a) => Show (Vector n a)

-- example showing we can't misuse these, comment it out
-- we claim it has five ints, but we only cons 4
--x :: Vector Five Int
--x = 0 `Cons` 1 `Cons` 2 `Cons` 3 `Cons` Nil

-- but this works
x :: Vector Five Int
x = 0 `Cons` 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil
-- we could use (:::) instead of Cons as an easier name
