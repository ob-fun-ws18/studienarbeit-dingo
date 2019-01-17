module P2PSockHost (
  startSockHost
) where

import P2PCommon
import P2PJson

import Data.Aeson as A

import System.IO
import Network.Socket
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

data SockCLientConnection = SockCLientConnection {
  member :: Member
, clientHandle :: Handle
}

data SockEvent = NewClient SockCLientConnection
               | SockInput SockCLientConnection String -- input from a sock
               | SockOutput String -- msg to output to all clients
               | SockDisconnect SockCLientConnection -- Sock disconnected

startSockHost :: Global -> Channels -> IO ThreadId
startSockHost glob chan = do
  putStrLn "startSockHost"
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet (toEnum (myHostPort glob)) iNADDR_ANY)
  listen sock 2
  forkIO $ runHostSock glob chan sock

runHostSock :: Global -> Channels -> Socket -> IO ()
runHostSock glob chans sock = do
  clientChan <- newChan
  -- sockhandlerId <- forkIO $ runSockEventHandler clientChan (csock chans) []
  acceptId <- forkIO $ runAcceptLoop sock clientChan
  loopHostHandler
  -- killThread sockhandlerId
  killThread acceptId

loopHostHandler :: IO ()
loopHostHandler = do 
  loopHostHandler

runAcceptLoop :: Socket -> Chan SockEvent -> IO ()
runAcceptLoop sock chan = do
  putStrLn "runAcceptLoop"
  (con, peer) <- accept sock
  putStrLn $ "New Connection: " ++ show peer
  hdl <- socketToHandle con ReadWriteMode
  hSetBuffering hdl NoBuffering
  eof <- hIsEOF hdl
  if not eof then do
    input <- B.hGetLine hdl
    let json = A.decode (BL.fromStrict input) :: Maybe JsonConnect
    case json of
      Just j -> do
        B.hPutStrLn hdl (BL.toStrict (A.encode (JsonResp 200)))
        let sockCon = SockCLientConnection (Member (jsUserName j) (jsUserId j) "" 0) hdl
        forkIO $ readClientConnection sockCon chan
        putStrLn "Fork new Client loop"
      Nothing -> putStrLn $ "Unknown connect msg: " ++ show input
  else 
    putStrLn "Unknown Client Connect, eof"
  runAcceptLoop sock chan

-- Handles Input from client sockets, has list of all Clients
runSockEventHandler :: Chan SockEvent -> Chan Event -> [SockCLientConnection] -> IO ()
runSockEventHandler chanClient chanHost members = do
  runSockEventHandler chanClient chanHost members


readClientConnection :: SockCLientConnection -> Chan SockEvent -> IO ()
readClientConnection client chan = do
  eof <- hIsEOF (clientHandle client)
  if eof then
    -- writeChan chan $ SockDisconnect client
    putStrLn "disconnect"
  else do
    input <- hGetLine (clientHandle client)
    putStrLn input
  -- TODO: Parse Input
    readClientConnection client chan
