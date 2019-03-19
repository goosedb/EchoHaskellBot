module Config where

import           Data.ByteString
import           Data.Text

data Proxy = Proxy
  { host :: ByteString
  , port :: Int
  }

data LogLevel
  = Debug
  | Warnings
  | Info

data Service
  = Telegram
  | Slack

data Config = Config
  { token    :: Text
  , proxy    :: Maybe Proxy
  , logLevel :: LogLevel
  , servise  :: Service
  }
