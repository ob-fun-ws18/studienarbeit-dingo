{-|
Module      : P2PChat.Common
Description : Defines common DataStructures and Helper Methods
Copyright   : (c) Chris Brammer, 2019
                  Wolfgang Gabler, 2019
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module P2PChat.Common where

import Data.Aeson

import GHC.Generics
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- | Events used to communicate via Channels
data Event = CmdInput String -- ^ Input from commandline
  | CmdQuit -- ^ Quit from commandline
  | CmdOutput String -- ^ String to print on commandline
  | SockHostConnect Member [Member] -- ^ Client Connect, updated list of members
  | SockHostDisconnect Member [Member] -- ^ Client Disconnect, updates list of member
  | SockMsgIn String String      -- ^ User, Msg sock -> main
  | SockMsgOut String      -- ^ User, Msg main -> sock
  | SockClientDisconnect -- ^ Client Mode: Socket disconnect
  deriving (Show, Eq)

-- | Checks if the event also forces a disconnect from the server
isDisconnect :: Event -> Bool
isDisconnect SockClientDisconnect = True
isDisconnect _ = False

-- | Checks if the event also forces an application quit
isQuit :: Event -> Bool
isQuit CmdQuit = True
isQuit _ = False

-- | Mode to Start in (Host or Client)
data StartMode = StartHost | StartClient String Int

-- | Config to start with
data StartConfig = StartConfig {
  username :: String
, hostPort :: Int
, mode :: StartMode
}

-- | Communication Channels used for thread communication
data Channels = Channels {
  cmain :: Chan Event
, cterm :: Chan Event
, csock :: Chan Event
}

-- | Global Data
data Global = Global {
  myUserName :: String
, myUUID :: String
, myHostPort :: Int
} deriving (Show, Eq)

-- | Chat Member description
data Member = Member {
  mUsername :: String
, mUUID :: String
, mHostname :: String
, mPort :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Generic JsonMessage
data JsonMessage = JsonMessage {
  jsType :: String,
  jsConnect :: Maybe JsonPayloadConnect,
  jsClientConnected :: Maybe JsonPayloadClientConnected,
  jsClientDisconnected :: Maybe JsonPayloadClientDisconnected,
  jsMessage :: Maybe JsonPayloadMessage
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- | JsonPayload when connection
data JsonPayloadConnect = JsonPayloadConnect {
  jspCname :: String,
  jspCuuid :: String,
  jspChostPort :: Int
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- | JsonPayload when a client connected
data JsonPayloadClientConnected = JsonPayloadClientConnected {
  jspCCmember :: Member,
  jspCCmembers :: [Member] 
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- | JsonPayload when a client disconnected
data JsonPayloadClientDisconnected = JsonPayloadClientDisconnected {
  jspCDmember :: Member,
  jspCDmembers :: [Member] 
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- | JsonPayload when sending a message
data JsonPayloadMessage = JsonPayloadMessage {
  jspMname :: String,
  jspMmsg :: String
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- JSON consctruction helpers
-- | Helper to Construct an empty Message
jsonEmpty :: String -> JsonMessage
jsonEmpty str = JsonMessage str Nothing Nothing Nothing Nothing

-- | Helper to Construct an "Ok" message
jsonOK :: JsonMessage
jsonOK = jsonEmpty "OK"

-- | Helper to Construct a Heartbeat
jsonHeartbeat :: JsonMessage
jsonHeartbeat = jsonEmpty "Heartbeat"

-- | Helper to Construct a connect Message
jsonConnect :: String -> String -> Int -> JsonMessage
jsonConnect name uuid port = JsonMessage "connect" (Just (JsonPayloadConnect name uuid port)) Nothing Nothing Nothing

-- | Helper to Construct a ClientConnected Message
jsonClientConnected :: Member -> [Member] -> JsonMessage
jsonClientConnected m ms = JsonMessage "clientConnected" Nothing (Just $ JsonPayloadClientConnected m ms) Nothing Nothing

-- | Helper to Construct a ClientDisconnected Message
jsonClientDisconnected :: Member -> [Member] -> JsonMessage
jsonClientDisconnected m ms = JsonMessage "clientDisconnected" Nothing Nothing (Just $ JsonPayloadClientDisconnected m ms) Nothing

-- | Helper to Construct a JsonMessage message
jsonMessageSend :: String -> String -> JsonMessage
jsonMessageSend user msg = JsonMessage "message" Nothing Nothing Nothing (Just $ JsonPayloadMessage user msg)

-- JSON parsing helpers
-- | Helper to Parse Json from incoming ByteString
jsonParse :: BL.ByteString -> Maybe JsonMessage
jsonParse bl = decode bl :: Maybe JsonMessage

-- | Helper to to check if its the JsonOK Message
isJsonOK :: JsonMessage -> Bool
isJsonOK (JsonMessage "OK" _ _ _ _) = True
isJsonOK _ = False
