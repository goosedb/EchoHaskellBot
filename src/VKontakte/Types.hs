module VKontakte.Types where

import           Bot
import           Data.Text
import           Model

data VKData = VKData
  { ts     :: !Int
  , server :: !Text
  } deriving (Show)

type VKModel = Model VKData
