{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Model where

import           Data.Text
import           Logger
import           Network.HTTP.Req
import           Types

data Model (service :: Service) = Model
  { service          :: !Service
  , token            :: !Text
  , httpConfig       :: !HttpConfig
  , logWriter        :: !LogWriter
  , helpMessage      :: !Text
  , defRepeatsNumber :: !Int
  , offset           :: !Int
  , state            :: !UserStates
  }
