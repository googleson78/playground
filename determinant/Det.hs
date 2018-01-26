module Det
    (det) 
    where

countinv :: [Int] -> Int
countinv [] = 0
countinv (x:xs) = (length $ filter (<x) xs) + countinv xs

perm :: [Int] -> [[Int]]
perm [] = [[]]
perm (x:xs) = concat $ map (insertAtAllPlaces x) $ perm xs

insertAtAllPlaces :: a -> [a] -> [[a]]
insertAtAllPlaces x [] = [[x]]
insertAtAllPlaces x ys = zipWith (insertAt x) [0..m] yss
                           where m = length ys + 1
                                 yss = replicate m ys

insertAt :: a -> Int -> [a] -> [a]
insertAt x 0 ys     = x:ys
insertAt x n (y:ys) = y:(insertAt x (n - 1) ys)

takePermInd :: [Int] -> [[Int]] -> [Int] 
takePermInd = zipWith (flip (!!))

det :: [[Int]] -> Int
det xss = sum permProducts 
                where n = length xss
                      numberPerms = perm [0..(n - 1)]

                      singleProdFormula = (\ perm ->
                                          ((-1) ^ (countinv perm)) *
                                          (product $ takePermInd perm xss))

                      permProducts = map singleProdFormula numberPerms
