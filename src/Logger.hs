{-# LANGUAGE DeriveGeneric #-}

module Logger where

import           Data.Aeson
import           GHC.Generics
import           Prelude      hiding (log)

data LogLevel
  = Debug
  | Warnings
  | Info
  deriving (Eq, Ord, Generic)

instance Show LogLevel where
  show Debug    = "[DEBUG]: "
  show Warnings = "[WARNING]: "
  show Info     = "[INFO]: "

instance FromJSON LogLevel

instance ToJSON LogLevel

createLogger :: LogLevel -> LogLevel -> String -> IO ()
createLogger loggerLogLevel = log
  where
    log :: LogLevel -> String -> IO ()
    log logLevel message =
      if logLevel >= loggerLogLevel
        then putStrLn $ show logLevel ++ message
        else return ()
