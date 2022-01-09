{-# LANGUAGE InstanceSigs #-}
module ParserMaybeVsList where
import Control.Applicative

-- a = char 'a'
-- twoAs = do
--   char 'a'
--   char 'a'
-- ex = do
--   twoAs <|> a
--   a
-- what do we want to happen for this example?
-- > runParser ex "aa"

-- with `Maybe` it fails, because the twoAs succeeds and we "throw away" the
-- alternative, but then when we try to get one more a, we have no input left
-- with `[]` it succeeds, because we keep all the potential parses right until the end
-- (with the Alternative instance (++)ing results)

-- this also means that the [] version is possibly slower, but for the
-- sake of convenience we use it here

-- repro:
-- >>> runParserMaybe ex "aa"
-- Nothing
ex :: ParserMaybe Char
ex = do
  twoAs <|> a
  a
twoAs :: ParserMaybe Char
twoAs = do
  a
  a
a :: ParserMaybe Char
a = charMaybe 'a'

-- >>> runParserList ex' "aa"
-- [("",'a')]
ex' :: ParserList Char
ex' = do
  twoAs' <|> a'
  a'
twoAs' :: ParserList Char
twoAs' = do
  a'
  a'
a' :: ParserList Char
a' = charList 'a'

newtype ParserMaybe a = MkParserMaybe {runParserMaybe :: String -> Maybe (String, a)}

instance Functor ParserMaybe where
  fmap :: (a -> b) -> ParserMaybe a -> ParserMaybe b
  fmap f p = MkParserMaybe $ fmap (fmap (fmap f)) $ runParserMaybe p

instance Applicative ParserMaybe where
  pure :: a -> ParserMaybe a
  pure x = MkParserMaybe $ \str -> Just (str, x)
  (<*>) :: ParserMaybe (a -> b) -> ParserMaybe a -> ParserMaybe b
  pf <*> px =
    MkParserMaybe $ \s ->
      case runParserMaybe pf s of
        Nothing -> Nothing
        Just (rest, f) ->
          fmap (fmap f) $ runParserMaybe px rest

instance Monad ParserMaybe where
  (>>=) :: ParserMaybe a -> (a -> ParserMaybe b) -> ParserMaybe b
  p >>= f =
    MkParserMaybe $ \s ->
      case runParserMaybe p s of
        Nothing -> Nothing
        Just (rest, res) ->
          runParserMaybe (f res) rest

instance Alternative ParserMaybe where
  empty :: ParserMaybe a
  empty = MkParserMaybe $ \_ -> Nothing
  px <|> py =
    MkParserMaybe $ \str ->
      case runParserMaybe px str of
        Nothing -> runParserMaybe py str
        Just res -> Just res

nomMaybe :: ParserMaybe Char
nomMaybe = MkParserMaybe $ \x ->
  case x of
    [] -> Nothing
    (c:cs) -> Just (cs, c)

charMaybe :: Char -> ParserMaybe Char
charMaybe exp = do
  x <- nomMaybe
  if x == exp
  then pure x
  else empty

newtype ParserList a = MkParserList {runParserList :: String -> [(String, a)]}

instance Functor ParserList where
  fmap :: (a -> b) -> ParserList a -> ParserList b
  fmap f p = MkParserList $ fmap (fmap (fmap f)) $ runParserList p

instance Applicative ParserList where
  pure :: a -> ParserList a
  pure x = MkParserList $ \str -> [(str, x)]
  (<*>) :: ParserList (a -> b) -> ParserList a -> ParserList b
  pf <*> px =
    MkParserList $ \s ->
      let ress = runParserList pf s
       in concatMap (\(rest, f) -> fmap (fmap f) $ runParserList px rest) ress

instance Monad ParserList where
  (>>=) :: ParserList a -> (a -> ParserList b) -> ParserList b
  p >>= f =
    MkParserList $ \s ->
      concatMap (\(rest, x) -> runParserList (f x) rest) $ runParserList p s


instance Alternative ParserList where
  empty :: ParserList a
  empty = MkParserList $ \_ -> []
  px <|> py =
    MkParserList $ \str ->
      runParserList px str ++ runParserList py str

nomList :: ParserList Char
nomList = MkParserList $ \x ->
  case x of
    [] -> []
    (c:cs) -> [(cs, c)]

charList :: Char -> ParserList Char
charList exp = do
  x <- nomList
  if x == exp
  then pure x
  else empty
