{-# LANGUAGE DeriveGeneric #-}

module Logger
  ( initLogger
  , writeLog
  , LogLevel(..)
  , Logger
  ) where

import           Control.Exception
import           Data.Aeson
import           GHC.Generics
import           Prelude           hiding (log)
import           System.IO

data Logger =
  Logger (LogLevel -> String -> String)
         Handle

data LogLevel
  = Debug
  | Warnings
  | Errors
  deriving (Eq, Ord, Generic)

instance Show LogLevel where
  show Debug    = "[ DEBUG ]: "
  show Warnings = "[WARNING]: "
  show Errors   = "[ ERROR ]: "

instance FromJSON LogLevel

instance ToJSON LogLevel

initLogger :: String -> LogLevel -> IO (Either String Logger)
initLogger "STDOUT" logLvl = return . Right $ createLogger stdout logLvl
initLogger path logLvl = io `catch` handler
  where
    handler :: IOError -> IO (Either String Logger)
    handler e = return $ Left $ show e
    io = do
      h <- openFile path AppendMode
      return $ Right $ createLogger h logLvl

createLogger :: Handle -> LogLevel -> Logger
createLogger out loggerLevel = Logger logger out
  where
    logger logLevel message
      | logLevel >= loggerLevel = show logLevel ++ message ++ "\n"
      | otherwise = []

writeLog :: Logger -> (LogLevel, String) -> IO ()
writeLog (Logger lgr handle) (lvl, msg) = hPutStr handle $ lgr lvl msg
