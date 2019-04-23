{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module VKontakte.Bot where

import           Bot
import           Control.Monad.IO.Class
import           VKontakte.Types

instance ServiceBot VKModel where
  bot = liftIO $ putStrLn "вэка"
