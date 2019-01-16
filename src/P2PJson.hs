{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module P2PJson where

import GHC.Generics
import Data.Aeson

-- Response for every Message
data JsonResp = JsonResp {
  jsState :: Int -- 200 OK, 500 Error
} deriving (Show, Generic, ToJSON, FromJSON)

-- Client -> Host, new User
data JsonConnect = JsonConnect {
  jsUserId :: String, -- UUID
  jsUserName :: String -- Username
} deriving (Show, Generic, ToJSON, FromJSON)

-- Client -> Host: new Message
-- Host -> Client: new Message
data JsonMsg = JsonMsg {
  jsName :: String, -- Username
  jsMsg :: String -- Message
} deriving (Show, Generic, ToJSON, FromJSON)

data JsonMember = JsonMember {
  jsMemName :: String
, jsMemUUID :: String
, jsMemHost :: String
, jsMemPort :: Int  
} deriving (Show, Generic, ToJSON, FromJSON)

-- Host -> Client on client connected
data JsonClientConnected = JsonClientConnected {
  jsCCmember :: JsonMember
, jsCCnewMembers :: [JsonMember]
} deriving (Show, Generic, ToJSON, FromJSON)

-- Host -> Client on client disconnected
data JsonClientDisconnected = JsonClientDisconnected {
  jsCDmember :: JsonMember
, jsCDnewMembers :: [JsonMember]  
} deriving (Show, Generic, ToJSON, FromJSON)
