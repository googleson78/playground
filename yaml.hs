{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import GHC.Generics
import Data.Text
import Data.Yaml
import Data.Yaml.TH
import Data.Aeson
import Data.Char (toLower)

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

data RecordsTest =
    RecordsTest
        { rtA :: Text
        , rtB :: Maybe Int
        }
    deriving (Show, Eq, Generic)

rtOpts :: Options
rtOpts = defaultOptions { fieldLabelModifier = process
                        , omitNothingFields  = True
                        }
    where process (x:y:z:xs) = Data.Char.toLower z : xs

instance ToJSON RecordsTest where
    toJSON = genericToJSON rtOpts

instance FromJSON RecordsTest where
    parseJSON = genericParseJSON rtOpts

data SingleConstrRecordTest =
    SingleConstrRecordTest
        { scrtA :: Text
        }
    deriving (Show, Eq, Generic)

scrtOpts :: Options
scrtOpts = defaultOptions { fieldLabelModifier = process
                          , unwrapUnaryRecords = True}
    where process (x:y:z:q:w:xs) = Data.Char.toLower w : xs

instance ToJSON SingleConstrRecordTest where
    toJSON = genericToJSON scrtOpts

instance FromJSON SingleConstrRecordTest where
    parseJSON = genericParseJSON scrtOpts

a = Paths ["/lolJK/kek", "najs, aaaa"]

asdf' :: Value
asdf' = [yamlQQ|a: lol|]

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
