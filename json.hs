{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Data.Aeson

import Data.ByteString.Lazy

import qualified Data.Text as T

data OmegaBigJson
    = OmegaBigJson
    { lessBig :: BigJson
    }
    deriving (Show, Eq, Generic)

instance ToJSON OmegaBigJson
instance FromJSON OmegaBigJson

data BigJson
    = BigJson
    { aLot :: String
    , ofStuff :: Int
    , andSomethingSmall :: SomethingSmall
    }
    deriving (Show, Eq, Generic)

instance ToJSON BigJson
instance FromJSON BigJson

data SomethingSmall
    = SomethingSmall
    { content :: T.Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON SomethingSmall
instance FromJSON SomethingSmall where
    parseJSON = withObject "SomethingSmall" $ \o -> do
        t <- o .: "content"
        if "something" `T.isPrefixOf` t
        then pure $ SomethingSmall t
        else fail $ T.unpack $ t <> " should start with \"something\"."

-- > eitherDecode a :: Either String OmegaBigJson
-- Left "Error in $.lessBig.andSomethingSmall.content: expected Text, encountered Number"
a :: ByteString
a = "{\"lessBig\":{\"ofStuff\":10,\"andSomethingSmall\":{\"content\":5},\"aLot\":\"asdf\"}}"

-- > eitherDecode b :: Either String OmegaBigJson
-- > Left "Error in $.lessBig.andSomethingSmall: doesntStartWithSomething should start with \"something\"."
b :: ByteString
b = "{\"lessBig\":{\"ofStuff\":10,\"andSomethingSmall\":{\"content\":\"doesntStartWithSomething\"},\"aLot\":\"asdf\"}}"
