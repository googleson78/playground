{-# LANGUAGE TemplateHaskell #-}

import Trie (Trie(..))
import qualified Trie as T
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Map.Lazy
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
    return (Node ma (fromList $ zip keys vals))


prop_insertWorks :: (Eq a) => String -> a -> Trie a -> Bool
prop_insertWorks key val trie = (T.lookup key $ T.insert key val trie) == (Just val)

prop_emptyTrieIsEmpty :: String -> Bool
prop_emptyTrieIsEmpty key = isNothing $ T.lookup key Empty

return []

test = $quickCheckAll
