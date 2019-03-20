{-# LANGUAGE DeriveGeneric #-}

module State where

import           Config       as C
import           Data.Text
import           GHC.Generics
import           Logger
import           Proxy

data State = State
  { token         :: !Text
  , proxy         :: !(Maybe Proxy)
  , logger        :: !Logger
  , helpMessage   :: !Text
  , repeatsNumber :: !Int
  } deriving (Generic)

stateFromConfig :: Logger -> Config -> State
stateFromConfig logger config =
  State
    (C.token config)
    (C.proxy config)
    logger
    (C.helpMessage config)
    (C.repeatsNumber config)
