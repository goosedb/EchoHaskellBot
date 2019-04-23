{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Logger

data Config = Config
  { token            :: !Text
  , proxy            :: !(Maybe Proxy)
  , service          :: !Service
  , logLevel         :: !LogLevel
  , helpMessage      :: !Text
  , defRepeatsNumber :: !Int
  , logStream        :: !Text
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data Proxy = Proxy
  { host :: !Text
  , port :: !Int
  } deriving (Show, Generic)

instance FromJSON Proxy where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data Service
  = Telegram
  | VKontakte
  deriving (Show, Generic)

instance FromJSON Service where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = eitherDecodeFileStrict
