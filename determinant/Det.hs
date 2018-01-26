module Det
    (det) 
    where

countinv :: [Int] -> Int
countinv [] = 0
countinv (x:xs) = length [z | z <- xs, x > z] + countinv xs

perm :: [Int] -> [[Int]]
perm [] = [[]]
perm (x:xs) = concat $ map (insertAll x) $ perm xs

insertAll :: a -> [a] -> [[a]]
insertAll x [] = [[x]]
insertAll x ys = zipWith (insertAt x) [0..m] yss
                    where m = length ys + 1
                          yss = replicate m ys

insertAt :: a -> Int -> [a] -> [a]
insertAt x 0 ys     = x:ys
insertAt _ _ []     = undefined
insertAt x n (y:ys) = y:(insertAt x (n - 1) ys)

takePermInd :: [Int] -> [[Int]] -> [Int] 
takePermInd = zipWith (flip (!!))

det :: [[Int]] -> Int
det [[x]] = x
det xss = sum [((-1) ^ (countinv perm)) * (product $ takePermInd perm xss) | perm <- perms]
                where n = length xss
                      perms = perm [0..(n - 1)]
