{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Utils where

import           Data.Text as T
import           Logger
import           Model
import           Telegram.Types

defaultTGData :: TGData
defaultTGData = TGData 0

tgModelFromConfig :: Config -> Logger -> TGModel
tgModelFromConfig = modelFromConfig defaultTGData

keyboard :: InlineKeyboardMarkup
keyboard =
  InlineKeyboardMarkup
    [ [ InlineKeyboardButton "1" "set1"
      , InlineKeyboardButton "2" "set2"
      , InlineKeyboardButton "3" "set3"
      , InlineKeyboardButton "4" "set4"
      , InlineKeyboardButton "5" "set5"
      ]
    ]

newPlainMessage :: Int -> Text -> MessageToSend
newPlainMessage id msg = MessageToSend id msg Nothing

newKeyboardMessage :: Int -> MessageToSend
newKeyboardMessage id =
  MessageToSend id "Choose repeats number: " $ Just keyboard

defUrl :: TGModel -> String -> String
defUrl model =
  (("https://api.telegram.org/bot" <> unpack (serviceToken model)) <>)

updateStates :: UserState -> (GetUpdate, TGModel) -> (GetUpdate, TGModel)
updateStates uState (upd, mdl@Model {userStates}) =
  (upd, mdl {userStates = newStates})
  where
    newStates = helper userStates
    helper [] = pure uState
    helper (u:us)
      | stateUserId u == stateUserId uState = uState : us
      | otherwise = u : helper us

updateOffset :: TGModel -> GetUpdate -> TGModel
updateOffset mdl upd = mdl {serviceData = TGData $ getUpdateId upd + 1}

parseCallbackData :: T.Text -> TGModel -> Int
parseCallbackData text Model {defSettings}
  | "set" `isPrefixOf` text =
    case reads $ unpack . T.drop 3 $ text of
      []       -> numOfRep defSettings
      [(n, _)] -> n
  | otherwise = numOfRep defSettings

logProcess :: TGModel -> [Log] -> IO ()
logProcess Model {writeLog} = mapM_ writeLog