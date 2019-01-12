module P2PCommon (
  Event(..)
) where

import Network.Socket

data RemoteHostInfo = RemoteHostInfo {
  rHostIp :: String,
  rHostPort :: PortNumber
} deriving (Show)

data Config = Config {
  username :: String, 
  hostPort :: PortNumber,
  connectTo :: Maybe RemoteHostInfo
} deriving (Show)

data Event = CmdInput String
           | CmdQuit
           | CmdOutput String
           | SockHostSub String         -- Client Connect
           | SockHostUnsub String       -- Client Disconnect
           | SockMsg String String      -- User, Msg
           deriving (Show, Eq)
