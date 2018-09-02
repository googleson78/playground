{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import GHC.Generics
import Data.Text
import Data.Yaml
import Data.Yaml.TH

data Config = Paths [Text]
            | Allowed [Text]
    deriving (Show, Eq, Generic)

instance FromJSON Config

instance ToJSON Config where

data A = A
    deriving (Show, Eq)

instance FromJSON A where
    parseJSON = withObject "test" $ \o -> do
        t <- o .:? "non" :: Parser (Maybe Value)
        pure (t `seq` A)

a = Paths ["/lolJK/kek", "najs, aaaa"]

b :: Value
b = [yamlQQ|
allowed:
    - one:
        - lol
        - kek
    - 2
    - three
projects:
    - BT
    - VMAX
|]

sample :: Value
sample = [yamlQQ|
modules-map:
    V-Ray:
        by-path-exact:
        - bla0
        - bla1
        - bla2
        by-path-contained:
        - foo
        by-project:
        - VRAY
    V-Ray GPU:

allowed-projects:
    - VRAY
    - PHI:
        components:
            - Core
verbose: true
|]

c = [yamlQQ|lol: "kek"|]
