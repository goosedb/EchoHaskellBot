{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Text as T
import           Data.Text.Encoding
import           GHC.Generics
import           Data.Aeson.Casing

data Proxy = Proxy
  { host :: !Text
  , port :: !Int
  } deriving (Show, Generic)

instance FromJSON Proxy
instance ToJSON Proxy

data LogLevel
  = Debug
  | Warnings
  | Info
  deriving (Show, Generic)

instance FromJSON LogLevel
instance ToJSON LogLevel

data Service
  = Telegram
  | Slack
  deriving (Show, Generic)

instance FromJSON Service
instance ToJSON Service

data Config = Config
  { token    :: !Text
  , proxy    :: !(Maybe Proxy)
  , logLevel :: !LogLevel
  , service  :: !Service
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase
instance ToJSON Config where
  toJSON = genericToJSON $ aesonDrop 0 $ snakeCase

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = do
  cfg <- L.readFile path
  return $ decode cfg 
