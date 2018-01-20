{-# LANGUAGE TemplateHaskell #-}

import Trie (Trie(..))
import qualified Trie as T
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Map.Lazy as M
import Data.Maybe

instance (Arbitrary a) => Arbitrary (Trie a) where
    arbitrary = do
        n <- choose (0, 4) :: Gen Int
        trie <- sized $ arbitraryTrie
        return $ if n == 0 then Empty 
                           else trie

arbitraryTrie :: Arbitrary a => Int -> Gen (Trie a)
arbitraryTrie m = do
    ma <- arbitrary
    n <- choose (0, m `div` 2)
    keys <- vectorOf n arbitrary
    vals <- vectorOf n $ arbitraryTrie $ m `div` 4
    return (Node ma (M.fromList $ zip keys vals))


-- testing if contains actually works
-- have circular dependency here, because contains
-- depends on insert working and vice versa
-- for the test to be valid
prop_contains :: (Eq a) => String -> a -> Trie a -> Bool
prop_contains key val = T.contains key val . T.insert key val 

-- testing if lookup in an empty Trie fails
prop_emptyTrieIsEmpty :: String -> Bool
prop_emptyTrieIsEmpty key = isNothing $ T.lookup key Empty

-- checking multiple insertion of unique keys for insert
prop_insertion :: (Eq a) => [String] -> [a] -> Trie a -> Property
prop_insertion keys vals trie = (not $ duplicate keys) ==> all (==True) contained

    where zipped           = zip keys vals
          insertZip        = flip $ uncurry T.insert
          inserted         = foldl insertZip trie zipped
          containsZip      = flip $ uncurry T.contains
          contained        = map (containsZip inserted) zipped
          duplicate []     = False
          duplicate (x:xs) = elem x xs || duplicate xs

-- checking multiple insertion of unique keys for insert'
prop_insertion' :: (Eq a) => [String] -> [a] -> Trie a -> Property
prop_insertion' keys vals trie = (not $ duplicate keys) ==> all (==True) contained

    where zipped           = zip keys vals
          insertZip        = flip $ uncurry T.insert'
          inserted         = foldl insertZip trie zipped
          containsZip      = flip $ uncurry T.contains
          contained        = map (containsZip inserted) zipped
          duplicate []     = False
          duplicate (x:xs) = elem x xs || duplicate xs

--insertZip :: Trie a -> (String, a) ->  Trie a
--insertZip trie (key, val) = T.insert key val trie

return []

test = $quickCheckAll
