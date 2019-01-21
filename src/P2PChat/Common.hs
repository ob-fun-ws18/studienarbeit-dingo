{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module P2PChat.Common where

import Data.Aeson

import GHC.Generics
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

data Event = CmdInput String -- Input from commandline
  | CmdQuit -- Quit from commandline
  | CmdOutput String -- String to print on commandline
  | SockHostConnect Member [Member] -- Client Connect, updated list of members
  | SockHostDisconnect Member [Member] -- Client Disconnect, updates list of member
  | SockMsgIn String String      -- User, Msg sock -> main
  | SockMsgOut String      -- User, Msg main -> sock
  | SockClientDisconnect -- Client Mode: Socket disconnect
  deriving (Show, Eq)

isDisconnect :: Event -> Bool
isDisconnect SockClientDisconnect = True
isDisconnect _ = False

isQuit :: Event -> Bool
isQuit CmdQuit = True
isQuit _ = False

data StartMode = StartHost | StartClient String Int
data StartConfig = StartConfig {
  username :: String
, hostPort :: Int
, mode :: StartMode
}

data Channels = Channels {
  cmain :: Chan Event
, cterm :: Chan Event
, csock :: Chan Event
}

data Global = Global {
  myUserName :: String
, myUUID :: String
, myHostPort :: Int
} deriving (Show, Eq)

data Member = Member {
  mUsername :: String
, mUUID :: String
, mHostname :: String
, mPort :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data JsonMessage = JsonMessage {
  jsType :: String,
  jsConnect :: Maybe JsonPayloadConnect,
  jsClientConnected :: Maybe JsonPayloadClientConnected,
  jsClientDisconnected :: Maybe JsonPayloadClientDisconnected,
  jsMessage :: Maybe JsonPayloadMessage
} deriving (Show, Generic, ToJSON, FromJSON)

data JsonPayloadConnect = JsonPayloadConnect {
  jspCname :: String,
  jspCuuid :: String,
  jspChostPort :: Int
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadClientConnected = JsonPayloadClientConnected {
  jspCCmember :: Member,
  jspCCmembers :: [Member] 
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadClientDisconnected = JsonPayloadClientDisconnected {
  jspCDmember :: Member,
  jspCDmembers :: [Member] 
} deriving (Show, Generic, ToJSON, FromJSON)
data JsonPayloadMessage = JsonPayloadMessage {
  jspMname :: String,
  jspMmsg :: String
} deriving (Show, Generic, ToJSON, FromJSON)

-- JSON consctruction helpers

jsonEmpty :: String -> JsonMessage
jsonEmpty str = JsonMessage str Nothing Nothing Nothing Nothing

jsonOK :: JsonMessage
jsonOK = jsonEmpty "OK"

jsonHeartbeat :: JsonMessage
jsonHeartbeat = jsonEmpty "Heartbeat"

jsonConnect :: String -> String -> Int -> JsonMessage
jsonConnect name uuid port = JsonMessage "connect" (Just (JsonPayloadConnect name uuid port)) Nothing Nothing Nothing

jsonClientConnected :: Member -> [Member] -> JsonMessage
jsonClientConnected m ms = JsonMessage "clientConnected" Nothing (Just $ JsonPayloadClientConnected m ms) Nothing Nothing

jsonClientDisconnected :: Member -> [Member] -> JsonMessage
jsonClientDisconnected m ms = JsonMessage "clientDisconnected" Nothing Nothing (Just $ JsonPayloadClientDisconnected m ms) Nothing

jsonMessageSend :: String -> String -> JsonMessage
jsonMessageSend user msg = JsonMessage "message" Nothing Nothing Nothing (Just $ JsonPayloadMessage user msg)

-- JSON parsing helpers

jsonParse :: BL.ByteString -> Maybe JsonMessage
jsonParse bl = decode bl :: Maybe JsonMessage

isJsonOK :: JsonMessage -> Bool
isJsonOK (JsonMessage "OK" _ _ _ _) = True
isJsonOK _ = False
