{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Yaml.TH

data Config = Paths [Text]
            | Allowed [Text]
    deriving (Show, Eq, Generic)

instance FromJSON Config

instance ToJSON Config where

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
