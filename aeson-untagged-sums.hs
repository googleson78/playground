{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T

data Param
  = StringParam T.Text
  | BoolParam Bool
  deriving (Show, Eq, Generic)

untaggedOptions :: Options
untaggedOptions = defaultOptions { sumEncoding = UntaggedValue }

instance FromJSON Param where
  parseJSON = genericParseJSON untaggedOptions

instance ToJSON Param where
  toJSON = genericToJSON untaggedOptions

-- > encode $ StringParam "lol"
-- "\"lol\""
-- > encode $ BoolParam True
-- "true"
-- > decode "true" :: Maybe Param
-- Just (BoolParam True)
-- > decode "\"true\"" :: Maybe Param
-- Just (StringParam "true")
