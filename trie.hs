import qualified Data.Map.Lazy as Lazy

data Trie a = Empty 
            | Node (Maybe a) (Lazy.Map Char (Trie a)) 
                deriving (Show, Eq)

instance Functor Trie where
    fmap g Empty = Empty
    fmap g (Node val map) = Node (g <$> val) ((fmap g) <$> map)

instance Foldable Trie where
    foldMap g Empty = mempty
    foldMap g (Node val map) = foldMap g val `mappend` foldMap (foldMap g) map
