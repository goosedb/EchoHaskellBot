module Bot where

import           Config
import           Data.Aeson
import           Data.Text.Encoding
import           GHC.Generics
import           Logger
import           Model
import           Network.HTTP.Client
import           Network.HTTP.Req

class Bot service where
  prepareModel :: service -> Logger -> Config -> Model service
  runBot :: Model service -> IO ()
