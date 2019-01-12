module P2PSocket (
  SocketConf,
  startSocket
) where

import           P2PCommon

import           System.IO
import           Network.Socket
import           Network.Socket.ByteString     as BS
import qualified Data.ByteString.Char8         as B
import qualified Control.Exception             as E
import qualified Data.Map                      as Map

import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

type Clients = (Map.Map String Handle)

data SocketConf = SocketConf {
  hostPort :: PortNumber
}

startSocket :: Chan Event -> Chan Event -> IO ()
startSocket cin cout = do
  putStrLn "Start Socket"

connect :: String -> PortNumber -> IO ()
connect ipAddr port = do
  putStrLn "LOG: connecting"
  