{-# LANGUAGE DeriveGeneric #-}

module Service where

import           Data.Aeson
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Req

data Service
  = Telegram
  | Slack
  deriving (Show, Generic)

instance FromJSON Service

instance ToJSON Service
