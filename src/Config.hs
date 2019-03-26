{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Logger
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Req    hiding (port)
import           Types

data Config = Config
  { token            :: !Text
  , proxy            :: !(Maybe Proxy)
  , logLevel         :: !LogLevel
  , service          :: !Service
  , helpMessage      :: !Text
  , defRepeatsNumber :: !Int
  , logStream        :: !Text
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

instance ToJSON Config where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = eitherDecodeFileStrict

createHttpConfig :: Maybe Proxy -> HttpConfig
createHttpConfig = addProxy
  where
    addProxy Nothing = defaultHttpConfig
    addProxy (Just p) =
      defaultHttpConfig
        { httpConfigProxy = Just $ Http.Proxy (encodeUtf8 $ host p) (port p)
        , httpConfigCheckResponse = \_ _ _ -> Nothing
        }
