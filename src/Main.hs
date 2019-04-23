{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Bot
import           Config
import           Control.Monad
import           Data.Text
import           Logger
import           Model
import           System.Environment
import           Telegram.Bot
import           Telegram.Utils
import           VKontakte.Bot
import           VKontakte.Utils

main :: IO ()
main = getArgs >>= processArgs >>= run

processArgs :: [String] -> IO (Either String Config)
processArgs []                  = return $ Left help
processArgs ("config_scheme":_) = return $ Left configScheme
processArgs ("help":_)          = return $ Left help
processArgs (configPath:_)      = loadConfig configPath

run :: Either String Config -> IO ()
run (Left msg) = putStrLn msg
run (Right config) = logger >>= runApp config
  where
    logger = initLogger (unpack $ logStream config) (logLevel config)
    runApp cfg@Config {service = Telegram} (Right lggr) =
      loop bot (tgModelFromConfig cfg lggr)
    runApp cfg@Config {service = VKontakte} (Right lggr) =
      loop bot (vkModelFromConfig cfg lggr)
    runApp _ (Left err) = putStrLn err

loop :: Bot model -> model -> IO ()
loop servBot mdl = do
  mdl' <- runBot servBot mdl
  loop servBot mdl'

help :: String
help =
  "I'm a bot. \n\
  \You can run me with a JSON-config by specifying one in argument.\n\
  \Also you can see config's scheme by running me with `config_scheme` in argument."

configScheme :: String
configScheme =
  "{\n\
  \\"proxy\": {\n\
  \    \"host\": \"%host%\",\n\
  \    \"port\": %integer%\n\
  \},\n\
  \\"token\": \"%token%\",\n\
  \\"service\": \"VKontakte | Telegram\",\n\
  \\"log_level\": \"Debug | Errors | Warnings\",\n\
  \\"log_stream\": \"STDOUT | %path_to_file%\",\n\
  \\"help_message\": \"foo\",\n\
  \\"repeat_message\": \"bar\",\n\
  \\"def_repeats_number\": %integer%\n\
  \}"
