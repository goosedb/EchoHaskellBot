{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module VKontakte.Bot
  ( bot
  , vkModelFromConfig
  ) where

import           Bot
import           Control.Monad.IO.Class
import           VKontakte.Types
import           VKontakte.Utils

instance ServiceBot VKModel where
  bot = liftIO $ putStrLn "вэка"
