{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Config              as Cfg
import           Data.Text           as Text
import           Data.Text.Encoding
import           GHC.Generics
import           Logger
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Req    as Req

data Model = Model
  { token         :: !Text
  , httpConfig    :: !Req.HttpConfig
  , logger        :: !Logger
  , helpMessage   :: !Text
  , repeatsNumber :: !Int
  , offset        :: !Int
  } deriving (Generic)

modelFromConfig :: Logger -> Config -> Model
modelFromConfig logger config =
  Model
    (Text.concat ["bot", Cfg.token config])
    (createHttpConfig $ Cfg.proxy config)
    logger
    (Cfg.helpMessage config)
    (Cfg.repeatsNumber config)
    (-1)

createHttpConfig :: Maybe Proxy -> Req.HttpConfig
createHttpConfig proxy = addProxy proxy $ Req.defaultHttpConfig
  where
    addProxy Nothing cfg = cfg
    addProxy (Just p) cfg =
      cfg
        { Req.httpConfigProxy =
            Just $ Http.Proxy (encodeUtf8 $ Cfg.host p) (Cfg.port p)
        , Req.httpConfigCheckResponse = \_ _ _ -> Nothing
        }
