module P2PChat.Socket.Host (
  startSocketHost
) where

import P2PChat.Common
import P2PChat.Socket

import Data.Aeson as A

import System.IO
import Network.Socket
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Timeout
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

data SockCLientConnection = SockCLientConnection {
  member :: Member
, clientHandle :: Handle
} deriving (Show, Eq)

data SockEvent = NewClient SockCLientConnection
               | SockInput SockCLientConnection String -- input from a sock
               | SockOutput String String -- msg to output to all clients
               | SockDisconnect SockCLientConnection -- Sock disconnected

startSocketHost :: Global -> Channels -> IO ThreadId
startSocketHost glob chan = do
  sock <- socket AF_INET Stream 0
  let addr = SockAddrInet (toEnum (myHostPort glob)) iNADDR_ANY
  bind sock addr
  listen sock 2
  forkIO $ runHostSock glob chan sock

runHostSock :: Global -> Channels -> Socket -> IO ()
runHostSock glob chans sock = do
  clientChan <- newChan :: IO (Chan SockEvent)
  clientMsgChan <- newChan :: IO (Chan JsonMessage)
  sockhandlerId <- forkIO $ runSockEventHandler clientChan (csock chans) clientMsgChan []
  acceptId <- forkIO $ runAcceptLoop sock clientChan clientMsgChan
  loopHostHandler glob chans clientMsgChan
  close sock
  killThread sockhandlerId
  killThread acceptId

loopHostHandler :: Global -> Channels -> Chan JsonMessage -> IO ()
loopHostHandler glob chans chanJson = do
  event <- readChan (csock chans)
  case event of
    s@(SockHostConnect _ _) -> writeChan (cmain chans) s
    s@(SockHostDisconnect _ _) -> writeChan (cmain chans) s
    s@(SockMsgIn _ _) -> writeChan (cmain chans) s
    SockMsgOut msg -> writeChan chanJson (jsonMessageSend (myUserName glob) msg)
    s@SockClientDisconnect -> writeChan (cmain chans) s
  loopHostHandler glob chans chanJson

runAcceptLoop :: Socket -> Chan SockEvent -> Chan JsonMessage -> IO ()
runAcceptLoop sock chan chanJson = do
  (con, peer) <- accept sock
  hdl <- socketToHandle con ReadWriteMode
  hSetBuffering hdl NoBuffering
  eof <- hIsEOF hdl
  when (not eof) $ do
    input <- B.hGetLine hdl
    let json = A.decode (BL.fromStrict input) :: Maybe JsonMessage
    case json of
      Just (JsonMessage "connect" (Just (JsonPayloadConnect n u p)) _ _ _) -> do
        B.hPutStrLn hdl (BL.toStrict (A.encode jsonOK))
        let sockCon = SockCLientConnection (Member n u "" p) hdl -- TODO: parse IP from socket
        writeChan chan (NewClient sockCon)
        clientChan <- dupChan chanJson
        forkIO $ runClientHandler sockCon clientChan chan
        putStrLn ""
      _ -> putStrLn $ "DEBUG: Unknown connect msg: " ++ show input
  runAcceptLoop sock chan chanJson

-- Handles Input from ALL client sockets, has list of all Clients
runSockEventHandler :: Chan SockEvent -> Chan Event -> Chan JsonMessage -> [SockCLientConnection] -> IO ()
runSockEventHandler chanClient chanHost chanClients members = do
  sockEvent <- readChan chanClient
  case sockEvent of
    NewClient c -> do 
      let newMembers = members ++ [c]
      writeChan chanClients (jsonClientConnected (member c) (map member newMembers))
      writeChan chanHost $ SockHostConnect (member c) (map member newMembers)
      runSockEventHandler chanClient chanHost chanClients newMembers
    SockInput c m -> do 
      writeChan chanHost $ SockMsgIn (mUsername $ member c) m
      writeChan chanClients (jsonMessageSend (mUsername $ member c) m)
      runSockEventHandler chanClient chanHost chanClients members
    SockOutput user msg -> do
      writeChan chanClients $ jsonMessageSend user msg
      runSockEventHandler chanClient chanHost chanClients members
    SockDisconnect c -> do
      let uuid = mUUID $ member c
      let newMembers = filter (\x -> mUUID (member x) /= uuid ) members
      writeChan chanClients (jsonClientDisconnected (member c) (map member newMembers))
      writeChan chanHost $ SockHostDisconnect (member c) (map member newMembers)
      runSockEventHandler chanClient chanHost chanClients newMembers

runClientHandler :: SockCLientConnection -> Chan JsonMessage -> Chan SockEvent -> IO ()
runClientHandler sock chanJson chanSockEvent = do
  outputId <- forkIO $ runSocketOutput (clientHandle sock) chanJson
  readClientConnection sock chanSockEvent
  killThread outputId
  hClose (clientHandle sock)
  writeChan chanSockEvent (SockDisconnect sock)

runSocketOutput :: Handle -> Chan JsonMessage -> IO ()
runSocketOutput hdl chan = do
  msg <- timeout 500000 $ readChan chan
  case msg of
    Just m -> B.hPutStrLn hdl $ BL.toStrict $ A.encode m
    Nothing -> B.hPutStrLn hdl $ BL.toStrict $ A.encode jsonHeartbeat
  runSocketOutput hdl chan

readClientConnection :: SockCLientConnection -> Chan SockEvent -> IO ()
readClientConnection client chan = do
  input <- timeout 1000000 $ readHandle (clientHandle client)
  case input of
    Just i ->
      case i of
        Just i -> do
          case jsonParse i of
            Just m -> handleInput client chan m
            Nothing -> putStrLn $ "DEBUG: Client Reading unknown msg"
          readClientConnection client chan
        Nothing -> return ()
    Nothing -> return ()

handleInput :: SockCLientConnection -> Chan SockEvent -> JsonMessage -> IO ()
handleInput client chan msg = case msg of
  (JsonMessage "message" _ _ _ (Just (JsonPayloadMessage user m))) -> writeChan chan (SockInput client m)
  _ -> return ()