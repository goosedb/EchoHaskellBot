{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Bot where

import           Config
import           Data.Aeson
import           GHC.Generics
import           Logger
import           Model
import           Types

class Bot (s :: Service) where
  prepareModel :: Logger -> Config -> Model s
  runBot :: Model s -> IO ()
