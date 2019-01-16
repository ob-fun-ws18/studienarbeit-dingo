module P2PCommon (
  Event(..)
, StartMode(..) 
, StartConfig(..)
, Channels(..)
, Global(..)
, Member(..)
) where

import Network.Socket
import Control.Concurrent.Chan

data Event = CmdInput String -- Input from commandline
           | CmdQuit -- Quit from commandline
           | CmdOutput String -- String to print on commandline
           | SockHostConnect String [Member] -- Client Connect, updated list of members
           | SockHostDisconnect String [Member] -- Client Disconnect, updates list of member
           | SockMsgIn String String      -- User, Msg sock -> main
           | SockMsgOut String      -- User, Msg main -> sock
           | SockClientDisconnect -- Client Mode: Socket disconnect
           deriving (Show, Eq)

data StartMode = StartHost | StartClient String Int
data StartConfig = StartConfig {
  username :: String
, hostPort :: Int
, mode :: StartMode
}

data Channels = Channels {
  cmain :: Chan Event
, ccmd :: Chan Event
, csock :: Chan Event
}

data Global = Global {
  myUserName :: String
, myUUID :: String
, myHostPort :: Int
}

data Member = Member {
  mUsername :: String
, mUUID :: String
, mHostname :: String
, mPort :: Int
} deriving (Show, Eq)
