{-|
Module      : P2PChat.Socket.Host
Description : Defines Host Implementation
Copyright   : (c) Chris Brammer, 2019
                  Wolfgang Gabler, 2019
-}

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

-- | Client Connection
data SockCLientConnection = SockCLientConnection {
  member :: Member
, clientHandle :: Handle
} deriving (Show, Eq)

-- | Socket Events
data SockEvent = NewClient SockCLientConnection
               | SockInput SockCLientConnection String -- input from a sock
               | SockOutput String String -- msg to output to all clients
               | SockDisconnect SockCLientConnection -- Sock disconnected

-- | Starts the Host Socket and a thread to manage it
startSocketHost :: Global -> Channels -> IO ThreadId
startSocketHost glob chan = do
  sock <- socket AF_INET Stream 0
  let addr = SockAddrInet (toEnum (myHostPort glob)) iNADDR_ANY
  bind sock addr
  listen sock 2
  forkIO $ runHostSock glob chan sock

-- | Sets up threads for the host. Thread to accept clients, thread to handle events
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

-- | Forwards events to the appropriate channels
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

-- | Accepts new clients and handle the Handshake between Client/Host
runAcceptLoop :: Socket -> Chan SockEvent -> Chan JsonMessage -> IO ()
runAcceptLoop sock chan chanJson = do
  (con, peer) <- accept sock
  hdl <- socketToHandle con ReadWriteMode
  hSetBuffering hdl NoBuffering
  eof <- hIsEOF hdl
  unless eof $ do
    input <- B.hGetLine hdl
    let json = A.decode (BL.fromStrict input) :: Maybe JsonMessage
    case json of
      Just (JsonMessage "connect" (Just (JsonPayloadConnect n u p)) _ _ _) -> do
        peerName <- getPeerName con
        let (addr, port) = fromSockAddr peerName
        B.hPutStrLn hdl (BL.toStrict (A.encode jsonOK))
        let sockCon = SockCLientConnection (Member n u addr port) hdl
        writeChan chan (NewClient sockCon)
        clientChan <- dupChan chanJson
        forkIO $ runClientHandler sockCon clientChan chan
        putStrLn ""
      _ -> putStrLn $ "DEBUG: Unknown connect msg: " ++ show input
  runAcceptLoop sock chan chanJson

-- | Handles Input from ALL client sockets, has list of all Clients
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

-- | Handles a single client connection, reading and writing
runClientHandler :: SockCLientConnection -> Chan JsonMessage -> Chan SockEvent -> IO ()
runClientHandler sock chanJson chanSockEvent = do
  outputId <- forkIO $ runSocketOutput (clientHandle sock) chanJson
  readClientConnection sock chanSockEvent
  killThread outputId
  hClose (clientHandle sock)
  writeChan chanSockEvent (SockDisconnect sock)

-- | Runs socket output, if we timeout we send a heartbeat
runSocketOutput :: Handle -> Chan JsonMessage -> IO ()
runSocketOutput hdl chan = do
  msg <- timeout 500000 $ readChan chan
  case msg of
    Just m -> B.hPutStrLn hdl $ BL.toStrict $ A.encode m
    Nothing -> B.hPutStrLn hdl $ BL.toStrict $ A.encode jsonHeartbeat
  runSocketOutput hdl chan

-- | Runs socket input, on timeout (this includes not receiving a heartbeat) returns
readClientConnection :: SockCLientConnection -> Chan SockEvent -> IO ()
readClientConnection client chan = do
  input <- timeout 1000000 $ readHandle (clientHandle client)
  case input of
    Just i ->
      case i of
        Just i -> do
          case jsonParse i of
            Just m -> handleInput client chan m
            Nothing -> putStrLn "DEBUG: Client Reading unknown msg"
          readClientConnection client chan
        Nothing -> return ()
    Nothing -> return ()

-- | Parses raw Input into JsonMessage and forwards to appropriate handler
handleInput :: SockCLientConnection -> Chan SockEvent -> JsonMessage -> IO ()
handleInput client chan msg = case msg of
  (JsonMessage "message" _ _ _ (Just (JsonPayloadMessage user m))) -> writeChan chan (SockInput client m)
  _ -> return ()