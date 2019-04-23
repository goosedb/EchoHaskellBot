{-# LANGUAGE OverloadedStrings #-}

module VKontakte.Utils where

import           Logger
import           Model
import           VKontakte.Types

defaultVKData :: VKData
defaultVKData = VKData 0 ""

vkModelFromConfig :: Config -> Logger -> VKModel
vkModelFromConfig = modelFromConfig defaultVKData
