{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module P2PJson where

import P2PCommon

import GHC.Generics
import Data.Aeson

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

data JsonMessage = JsonMessage {
  jsType :: String,
  jsConnect :: Maybe JsonPayloadConnect,
  jsClientConnected :: Maybe JsonPayloadClientConnected,
  jsClientDisconnected :: Maybe JsonPayloadClientDisconnected,
  jsMessage :: Maybe JsonPayloadMessage
} deriving (Show, Generic, ToJSON, FromJSON)

data JsonMember = JsonMember {
  jsMemName :: String
, jsMemUUID :: String
, jsMemHost :: String
, jsMemPort :: Int -- Port for future Host Service
} deriving (Show, Generic, ToJSON, FromJSON)

data JsonPayloadConnect = JsonPayloadConnect {
  jspCname :: String,
  jspCuuid :: String,
  jspChostPort :: Int
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadClientConnected = JsonPayloadClientConnected {
  jspCCmember :: JsonMember,
  jspCCmembers :: [JsonMember] 
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadClientDisconnected = JsonPayloadClientDisconnected {
  jspCDmember :: JsonMember,
  jspCDmembers :: [JsonMember] 
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadMessage = JsonPayloadMessage {
  jspMname :: String,
  jspMmsg :: String
} deriving (Show, Generic, ToJSON, FromJSON)

jsonEmpty :: String -> JsonMessage
jsonEmpty str = JsonMessage str Nothing Nothing Nothing Nothing

jsonOK :: JsonMessage
jsonOK = jsonEmpty "OK"

jsonHeartbeat :: JsonMessage
jsonHeartbeat = jsonEmpty "Heartbeat"

jsonConnect :: String -> String -> Int -> JsonMessage
jsonConnect name uuid port = JsonMessage "connect" (Just (JsonPayloadConnect name uuid port)) Nothing Nothing Nothing

isJsonOK :: JsonMessage -> Bool
jsJsonOK (JsonMessage "OK" _ _ _ _) = True
isJsonOK _ = False

jsonMessageSend :: String -> String -> JsonMessage
jsonMessageSend user msg = JsonMessage "message" Nothing Nothing Nothing (Just $ JsonPayloadMessage user msg)

jsonParse :: BL.ByteString -> Maybe JsonMessage
jsonParse bl = decode bl :: Maybe JsonMessage

jsonParseMember :: JsonMember -> Member
jsonParseMember (JsonMember n u h p) = Member n u h p

jsonParseMember' :: Member -> JsonMember
jsonParseMember' (Member n u h p) = JsonMember n u h p

jsonClientConnected :: Member -> [Member] -> JsonMessage
jsonClientConnected m ms = JsonMessage "clientConnected" Nothing (Just $ JsonPayloadClientConnected (jsonParseMember' m) (map jsonParseMember' ms)) Nothing Nothing

jsonClientDisconnected :: Member -> [Member] -> JsonMessage
jsonClientDisconnected m ms = JsonMessage "clientDisconnected" Nothing Nothing (Just $ JsonPayloadClientDisconnected (jsonParseMember' m) (map jsonParseMember' ms)) Nothing