module Det
    (det) 
    where

-- counts the inversions of a list
-- in the most "wooden" way possible
-- (using the definition)
countinv :: [Int] -> Int
countinv [] = 0
countinv (x:xs) = (length $ filter (<x) xs) + countinv xs

-- all permutations of a list
-- again, very basic way 
-- a permutation of (x:xs) is inserting
-- x at every possible "spot", and that is
-- for each permutation of xs
permute :: [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = concat $ map (insertAtAllPlaces x) $ permute xs

insertAtAllPlaces :: a -> [a] -> [[a]]
insertAtAllPlaces x [] = [[x]]
insertAtAllPlaces x ys = zipWith (insertAt x) [0..m] yss
                           where m = length ys + 1
                                 yss = replicate m ys

-- as required by permute
insertAt :: a -> Int -> [a] -> [a]
insertAt x 0 ys     = x:ys
insertAt x n (y:ys) = y:(insertAt x (n - 1) ys)
insertAt _ _ _ = undefined

-- purely mechanical thing required for dets 
-- when making a single product of the sum of the entire det
-- 
-- take a list of indices and uses them to index
-- each of the lists in a list
takePermInd :: [Int] -> [[a]] -> [a] 
takePermInd = zipWith (flip (!!))

det :: [[Int]] -> Int
det xss = sum permProducts 
                where n = length xss
                      numberPerms = permute [0..(n - 1)]

                      singleProdFormula = (\ perm ->
                                          ((-1) ^ (countinv perm)) *
                                          (product $ takePermInd perm xss))

                      permProducts = map singleProdFormula numberPerms
