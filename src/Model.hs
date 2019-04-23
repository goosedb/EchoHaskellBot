module Model
  ( Model(..)
  , DefaultSettings(..)
  , numOfRep
  , helpMsg
  , UserState(..)
  , UserStates
  {- Re-Exports from Config -}
  , Service
  , Config
  , modelFromConfig
  ) where

import           Bot
import           Config
import           Data.ByteString.Char8       as BS
import           Data.Text
import           Data.Text.Encoding
import           Logger
import           Network.HTTP.Client.Conduit as HTTP

modelFromConfig :: serviceData -> Config -> Logger -> Model serviceData
modelFromConfig sd cfg logger =
  Model
    sd
    (token cfg)
    (createWriter logger)
    (mbproxy >>= \p ->
       return $ HTTP.Proxy (encodeUtf8 $ Config.host p) (Config.port p))
    (DefaultSettings
       (defRepeatsNumber cfg)
       (helpMessage cfg))
    []
  where
    mbproxy = Config.proxy cfg

data Model a = Model
  { serviceData  :: !a
  , serviceToken :: !Text
  , writeLog     :: !LogWriter
  , proxy        :: !(Maybe HTTP.Proxy)
  , defSettings  :: !DefaultSettings
  , userStates   :: !UserStates
  }

data DefaultSettings = DefaultSettings
  { numOfRep :: !Int
  , helpMsg  :: !Text
  }

type UserStates = [UserState]

data UserState = UserState
  { stateUserId   :: !Int
  , stateNumOfRep :: !Int
  } deriving (Show)
