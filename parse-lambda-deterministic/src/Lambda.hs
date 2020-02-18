{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Lambda where

import Data.Foldable (asum)
import Data.Char
import Data.Functor (void)
import Parser

newtype Name = Name {getName :: String}
  deriving newtype (Eq, Show)

sat :: (Char -> Maybe a) -> Parser a
sat f = do
  Just x <- f <$> nom
  pure x

onBool :: (a -> Bool) -> a -> Maybe a
onBool p x = if p x then Just x else Nothing

ws :: Parser ()
ws = void $ many $ sat $ onBool isSpace

lexeme :: Parser a -> Parser a
lexeme = (<* ws)

char :: Char -> Parser ()
char c = void $ lexeme $ sat \x -> if x == c then Just () else Nothing

data Lambda
  = Var Name
  | App Lambda Lambda
  | Lam Name Lambda
  deriving (Eq, Show)

name :: Parser Name
name = lexeme $ sat \x -> if x `elem` "xyzuvw" then Just $ Name [x] else Nothing

var :: Parser Lambda
var = Var <$> name

lambda :: Parser Lambda
lambda =
  asum
    [ lam
    , app
    , var
    ]

lambdaAll :: Parser Lambda
lambdaAll = lambda <* endOfInput

bracket :: Parser a -> Parser a
bracket p = some (char '(') *> p <* some (char ')')

app :: Parser Lambda
app = foldl1 App <$> do some $ var <|> lam <|> bracket app

lam :: Parser Lambda
lam = do
  char '\\'
  variable <- name
  char '.'
  body <- lambda
  pure $ Lam variable body
