
import Data.Char (ord)
import System.IO
import System.Posix

import qualified Data.ByteString.Lazy.Char8 as BS

type Seed = Int

getSeed :: IO Int
getSeed = do
  contents <- BS.readFile "/dev/urandom"
  pure $ sum $ map ord $ take 8 $ BS.unpack contents

next :: Seed -> (Seed, Int)
next input = (input * 10 + 5, input + 1)

salt :: String -> Seed -> (Seed, String)
salt password seed = (nextSeed, password ++ show randomNumber)
  where
    (nextSeed, randomNumber) = next seed

nNumbers :: Int -> Seed -> (Seed, [Int])
nNumbers 0 lastSeed = (lastSeed, [])
nNumbers n seed =
  let (nextSeed, randomNumber) = next seed
      (lastSeed, numbers) = nNumbers (n - 1) nextSeed
   in (lastSeed, randomNumber : numbers)

newtype Randomness a = Randomness { runRandomness :: (Seed -> (Seed, a)) }

num :: Randomness Int
num = Randomness next

saltR :: String -> Randomness String
saltR password = Randomness $
  \seed -> let (nextSeed, randomNumber) = next seed
            in (nextSeed, password ++ show randomNumber)

instance Functor Randomness where
  fmap f randomA = Randomness $ \seed ->
    let g = runRandomness randomA
        (newSeed, a) = g seed
     in (newSeed, f a)

-- applicative
-- why fmap doesn't work (need to "lift twice")
-- what more do we need
genPair :: Randomness a -> Randomness b -> Randomness (a, b)
genPair genA genB = Randomness $ \seed ->
  let (newSeed, x) = runRandomness genA seed
      (lastSeed, y) = runRandomness genB newSeed
   in (lastSeed, (,) x y)

multRandomN :: Int -> Randomness Int
multRandomN 0 = Randomness $ \seed -> (seed, 1)
multRandomN n = Randomness $ \seed ->
  let (newSeed, number) = runRandomness num seed
      (lastSeed, rest) = runRandomness (multRandomN (n - 1)) newSeed
   in (lastSeed, number * rest)

genRandomN :: Randomness a -> Int -> Randomness [a]
genRandomN genOne 0 = Randomness $ \seed -> (seed, [])
genRandomN genOne n = Randomness $ \seed ->
  let (newSeed, a) = runRandomness genOne seed
      (lastSeed, as) = runRandomness (genRandomN genOne (n - 1)) newSeed
   in (lastSeed, a:as)

noAction :: a -> Randomness a
noAction x = Randomness $ \seed -> (seed, x)

lift2 :: (a -> b -> c) -> Randomness a -> Randomness b -> Randomness c
lift2 = undefined

-- using liftA2
lift3 :: (a -> b -> c -> d) -> Randomness a -> Randomness b -> Randomness c -> Randomness d
lift3 = undefined

splat :: Randomness (a -> b) -> Randomness a -> Randomness b
splat = undefined

-- using splat
lift3' :: (a -> b -> c -> d) -> Randomness a -> Randomness b -> Randomness c -> Randomness d
lift3' = undefined

-- monad???
-- why splat doesn't work (conditional execution)
-- what more do we need
genRandom :: Randomness a -> Randomness [a]
genRandom genOne = Randomness $ \seed ->
  let (newSeed, n) = runRandomness num seed
   in runRandomness (genRandomN genOne n) newSeed

numPredicate :: (Int -> Bool) -> Randomness Int
numPredicate p = Randomness $ \seed ->
  let (newSeed, n) = runRandomness num seed
   in if p n
      then (newSeed, n)
      else runRandomness (numPredicate p) newSeed

using :: (a -> Randomness b) -> Randomness a -> Randomness b
using randomnessF ra = Randomness $ \seed ->
  let (newSeed, a) = runRandomness ra seed
   in runRandomness (randomnessF a) newSeed
