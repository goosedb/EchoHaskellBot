{-# LANGUAGE DeriveGeneric #-}

module Proxy where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Proxy = Proxy
  { host :: !Text
  , port :: !Int
  } deriving (Show, Generic)

instance FromJSON Proxy

instance ToJSON Proxy
