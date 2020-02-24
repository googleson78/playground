{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

import GHC.Generics (Generic, Rep)
import Data.Aeson
import qualified Data.Text as T

data Param
  = StringParam T.Text
  | BoolParam Bool
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Untagged Param

-- Untagged
untaggedOptions :: Options
untaggedOptions = defaultOptions { sumEncoding = UntaggedValue }

newtype Untagged a = Untagged { getUntagged :: a }
  deriving stock (Eq, Generic)

instance (GFromJSON Zero (Rep a), Generic a) => FromJSON (Untagged a) where
  parseJSON = fmap Untagged . genericParseJSON untaggedOptions

instance (Generic a, (GToJSON Zero (Rep a))) => ToJSON (Untagged a) where
  toJSON = genericToJSON untaggedOptions . getUntagged
