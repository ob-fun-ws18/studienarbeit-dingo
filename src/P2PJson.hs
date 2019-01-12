{-# LANGUAGE DeriveGeneric #-}

module P2PJson where

import GHC.Generics
import Data.Aeson

data JsonResp = JsonResp {
  jsState :: Int
} deriving (Generic)

instance ToJSON JsonResp
instance FromJSON JsonResp

data JsonConnect = JsonConnect {
  jsUserId :: String
} deriving (Generic)

instance ToJSON JsonConnect
instance FromJSON JsonConnect

data JsonMsg = JsonMsg {
  jsName :: String,
  jsMsg :: String
} deriving (Generic)

instance ToJSON JsonMsg
instance FromJSON JsonMsg

data JsonClientList = JsonClientList {
  cliets :: [String]
} deriving (Generic)

instance ToJSON JsonClientList
instance FromJSON JsonClientList
