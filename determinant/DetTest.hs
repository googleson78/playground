{-# LANGUAGE TemplateHaskell #-}

import Det
import Test.QuickCheck

newtype SquareM = Sq [[Int]] deriving (Show)

instance Arbitrary SquareM where
    arbitrary = sized $ fixedSizeSq

fixedSizeSq :: Int -> Gen SquareM
fixedSizeSq m = do
    lists <- vectorOf m $ vectorOf m arbitrary
    return $ Sq lists

-- limit size to 7 because 9 is possibly more than
-- a minute, and 8 is still too slow
-- we also need to increase retry ratio because
-- we won't even reach 100 tests otherwise
args = Args Nothing 500 50 7 True 9223372036854775807

-- swapping two rows of a matrix should reverse the sign of the det
prop_swapRows :: Int -> Int -> SquareM -> Property
prop_swapRows n k sq = inBounds n && inBounds k && (n /= k) ==> (det $ swapRows n k xss) == ((-1) * det xss)
    where xss = (\(Sq x) -> x) sq
          m = length xss
          inBounds x = (0 <= x) && (x < m)

-- stackexchange, had 0.8% battery
-- but also seems reasonable seeing as how 
-- this is only done once and is reasonably fast
swapRows :: Int -> Int -> [a] -> [a]
swapRows first second xs = zipWith (\x y ->
    if x == first then xs !! second
    else if x == second then xs !! first
    else y) [0..] xs

return []
test = $forAllProperties (quickCheckWithResult args)
