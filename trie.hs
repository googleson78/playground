import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe

data Trie a = Empty 
            | Node (Maybe a) (Map Char (Trie a)) 
                deriving (Show, Eq)

instance Functor Trie where
    fmap g Empty = Empty
    fmap g (Node val map) = Node (g <$> val) ((fmap g) <$> map)

instance Foldable Trie where
    foldMap g Empty = mempty
    foldMap g (Node val map) = foldMap g val `mappend` foldMap (foldMap g) map

-- abstract above the value of a Node, so I can
-- write two cases as one in insert
nodeVal :: Trie a -> Maybe a
nodeVal Empty        = Nothing
nodeVal (Node val _) = val

-- same as above
nodeMap :: Trie a -> Map Char (Trie a)
nodeMap Empty        = Map.empty
nodeMap (Node _ map) = map

-- manually looking at cases
insert :: String -> a -> Trie a -> Trie a
insert [] val Empty                  = Node (Just val) Map.empty
insert [] val (Node currval map)     = Node (Just val) map

insert (x:xs) val Empty              = Node Nothing (Map.insert x (insert xs val Empty) Map.empty)
insert (x:xs) val (Node currval map) = Node currval inserted
                                        where next     = fromMaybe Empty $ Map.lookup x map
                                              inserted = Map.insert x (insert xs val next) map

-- recursing only on the string
insert' :: String -> a -> Trie a -> Trie a
insert' [] val tree     = Node (Just val) $ nodeMap tree
insert' (x:xs) val tree = Node (nodeVal tree) inserted
                           where map      = nodeMap tree
                                 next     = fromMaybe Empty $ Map.lookup x map
                                 inserted = Map.insert x (insert' xs val next) map
