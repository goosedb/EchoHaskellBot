{-# LANGUAGE DeriveGeneric #-}

module State where

import           Config              as Cfg
import           Data.Text
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Logger
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Req    as Req

data State = State
  { token         :: !Text
  , httpConfig    :: !Req.HttpConfig
  , logger        :: !Logger
  , helpMessage   :: !Text
  , repeatsNumber :: !Int
  , offset        :: !Int
  } deriving (Generic)

stateFromConfig :: Logger -> Config -> State
stateFromConfig logger config =
  State
    (Cfg.token config)
    (createHttpConfig $ Cfg.proxy config)
    logger
    (Cfg.helpMessage config)
    (Cfg.repeatsNumber config)
    0

createHttpConfig :: Maybe Proxy -> Req.HttpConfig
createHttpConfig proxy = addProxy proxy $ Req.defaultHttpConfig
  where
    addProxy Nothing cfg = cfg
    addProxy (Just p) cfg =
      cfg
        { Req.httpConfigProxy =
            Just $ Http.Proxy (encodeUtf8 $ Cfg.host p) (Cfg.port p)
        }
