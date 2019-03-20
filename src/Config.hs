{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text
import           GHC.Generics
import           Logger

data Proxy = Proxy
  { host :: !Text
  , port :: !Int
  } deriving (Show, Generic)

instance FromJSON Proxy

instance ToJSON Proxy

data Service
  = Telegram
  | Slack
  deriving (Show, Generic)

instance FromJSON Service

instance ToJSON Service

data Config = Config
  { token         :: !Text
  , proxy         :: !(Maybe Proxy)
  , logLevel      :: !LogLevel
  , service       :: !Service
  , helpMessage   :: !Text
  , repeatsNumber :: !Int
  , logStream     :: Text
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

instance ToJSON Config where
  toJSON = genericToJSON $ aesonDrop 0 $ snakeCase

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = eitherDecodeFileStrict
