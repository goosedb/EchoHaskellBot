{-# LANGUAGE DeriveGeneric #-}

module Logger
  ( createLogger
  , writeLog
  , LogLevel
  ) where

import           Data.Aeson
import           GHC.Generics
import           Prelude      hiding (log)

data Logger =
  Logger (LogLevel -> String -> String)
         Output

data Output
  = Stdout
  | File FilePath

data LogLevel
  = Debug
  | Warnings
  | Errors
  deriving (Eq, Ord, Generic)

instance Show LogLevel where
  show Debug    = "[DEBUG]: "
  show Warnings = "[WARNING]: "
  show Errors   = "[ERROR]: "

instance FromJSON LogLevel

instance ToJSON LogLevel

createLogger :: String -> LogLevel -> Logger
createLogger out loggerLevel = Logger logger stream
  where
    logger logLevel message
      | logLevel >= loggerLevel = show logLevel ++ message ++ "\n"
      | otherwise = []
    stream
      | out == "STDOUT" = Stdout
      | otherwise = File out

writeLog :: Logger -> LogLevel -> String -> IO ()
writeLog (Logger lgr (File path)) lvl msg = appendFile path $ lgr lvl msg
writeLog (Logger lgr Stdout) lvl msg      = putStr $ lgr lvl msg
