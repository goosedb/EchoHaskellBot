{-# LANGUAGE DeriveGeneric #-}

module Model where

import           Config              as Cfg
import           Data.Text           as Text
import           Data.Text.Encoding
import           GHC.Generics
import           Logger
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Req    as Req

type UserStates = [UserState]

data Model service = Model
  { service          :: !service
  , token            :: !Text
  , httpConfig       :: !Req.HttpConfig
  , logWriter        :: !LogWriter
  , helpMessage      :: !Text
  , defRepeatsNumber :: !Int
  , offset           :: !Int
  , state            :: ![UserState]
  } deriving (Generic)

data UserState = UserState
  { id     :: !Int
  , repNum :: !Int
  }

createHttpConfig :: Maybe Proxy -> Req.HttpConfig
createHttpConfig proxy = addProxy proxy Req.defaultHttpConfig
  where
    addProxy Nothing cfg = cfg
    addProxy (Just p) cfg =
      cfg
        { Req.httpConfigProxy =
            Just $ Http.Proxy (encodeUtf8 $ Cfg.host p) (Cfg.port p)
        , Req.httpConfigCheckResponse = \_ _ _ -> Nothing
        }
