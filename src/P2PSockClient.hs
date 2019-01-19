module P2PSockClient (
  startSockClient
) where

import P2PCommon 
import P2PJson

import Data.Aeson as A

import Data.Maybe
import System.IO
import System.Timeout
import Network.Socket
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- TODO: return Maybe, nothing on connection error
startSockClient :: String -> Int -> Global -> Channels -> IO (ThreadId)
startSockClient host port glob chans = do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  succ <- handshake hdl (myUserName glob) (myUUID glob) (myHostPort glob)
  forkIO $ runClientSock glob chans hdl

handshake :: Handle -> String -> String -> Int -> IO Bool
handshake hdl name uuid port = do
  B.hPutStrLn hdl (BL.toStrict (A.encode (jsonConnect uuid name port)))
  eof <- hIsEOF hdl
  if eof then 
    return False
  else do
    input <- B.hGetLine hdl
    let json = A.decode (BL.fromStrict input) :: Maybe JsonMessage
    case json of
      Just j -> return $ isJsonOK j
      Nothing -> return False

runClientSock :: Global -> Channels -> Handle -> IO()
runClientSock glob chans handle = do
  readId <- forkIO $ readClientSock handle (csock chans)
  outputChan <- newChan
  outputId <- forkIO $ runSocketOutput handle outputChan
  loopClientSock glob chans handle outputChan
  hClose handle
  killThread readId
  killThread outputId

loopClientSock :: Global -> Channels -> Handle -> Chan JsonMessage -> IO()
loopClientSock glob chans handle chan = do
  event <- readChan (csock chans)
  case event of
    s@(SockHostConnect _ _) -> writeChan (cmain chans) s
    s@(SockHostDisconnect _ _) -> writeChan (cmain chans) s
    s@(SockMsgIn _ _) -> writeChan (cmain chans) s
    SockMsgOut msg -> writeChan chan (jsonMessageSend "" msg)
    s@SockClientDisconnect -> writeChan (cmain chans) s
  unless (isDisconnect event) $ loopClientSock glob chans handle chan

isDisconnect :: Event -> Bool
isDisconnect SockClientDisconnect = True
isDisconnect _ = False

readClientSock :: Handle -> Chan Event -> IO ()
readClientSock hdl chan = do
  input <- timeout 1000000 $ readHandle hdl
  case input of
    Just i ->
      case i of
        Just i -> do
          case jsonParse i of
            Just m -> handleInput chan m
            Nothing -> putStrLn $ "LOG: Client Reading unknown msg"
          readClientSock hdl chan
        Nothing -> writeChan chan SockClientDisconnect -- disconnect
    Nothing -> writeChan chan SockClientDisconnect -- timeout  

handleInput :: Chan Event -> JsonMessage -> IO ()
handleInput chan msg = case msg of
  (JsonMessage "message" _ _ _ (Just (JsonPayloadMessage user m))) -> writeChan chan (SockMsgIn user m)
  (JsonMessage "clientConnected" _ (Just (JsonPayloadClientConnected m ms)) _ _) -> writeChan chan (SockHostConnect (jsonParseMember m) [])
  (JsonMessage "clientDisconnected" _ _ (Just (JsonPayloadClientDisconnected m ms)) _) -> writeChan chan (SockHostDisconnect (jsonParseMember m) [])
  _ -> return ()
  


-- read handle, return Nothing on eof
readHandle :: Handle -> IO (Maybe BL.ByteString)
readHandle handle = do
  eof <- hIsEOF handle
  if eof then
    return Nothing
  else do
    input <- B.hGetLine handle
    return $ Just $ BL.fromStrict input

runSocketOutput :: Handle -> Chan JsonMessage -> IO ()
runSocketOutput hdl chan = do
  msg <- timeout 500000 $ readChan chan
  case msg of
    Just m -> B.hPutStrLn hdl $ BL.toStrict $ A.encode m
    Nothing -> B.hPutStrLn hdl $ BL.toStrict $ A.encode jsonHeartbeat
  runSocketOutput hdl chan