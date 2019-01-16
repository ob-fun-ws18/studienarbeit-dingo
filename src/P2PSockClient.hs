module P2PSockClient (
  startSockClient
) where

import P2PCommon 
import P2PJson

import Data.Aeson as A

import Data.Maybe
import System.IO
import Network.Socket
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

startSockClient :: String -> Int -> Global -> Channels -> IO (ThreadId)
startSockClient host port glob chans = do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  succ <- handshake hdl (myUserName glob) (myUUID glob)
  forkIO $ runClientSock glob chans hdl

handshake :: Handle -> String -> String -> IO Bool
handshake hdl name uuid = do
  B.hPutStrLn hdl (BL.toStrict (A.encode (JsonConnect uuid name)))
  eof <- hIsEOF hdl
  if eof then 
    return False
  else do
    input <- B.hGetLine hdl
    let json = A.decode (BL.fromStrict input) :: Maybe JsonResp
    return $ checkResp json

checkResp :: Maybe JsonResp -> Bool
checkResp (Just j) = (jsState j) == 200
checkResp Nothing = False      

runClientSock :: Global -> Channels -> Handle -> IO()
runClientSock glob chans handle = do
  readId <- forkIO $ readClientSock handle (csock chans)
  loopClientSock glob chans handle
  hClose handle
  killThread readId

loopClientSock :: Global -> Channels -> Handle -> IO()
loopClientSock glob chans handle = do
  event <- readChan (csock chans)
  case event of
    s@(SockHostConnect _ _) -> writeChan (cmain chans) s
    s@(SockHostDisconnect _ _) -> writeChan (cmain chans) s
    s@(SockMsgIn _ _) -> writeChan (cmain chans) s
    SockMsgOut msg -> B.hPutStrLn handle (BL.toStrict (A.encode (JsonMsg "" msg))) -- user name not relevant since it will be populated by host
    s@SockClientDisconnect -> writeChan (cmain chans) s
  unless (isDisconnect event) $ loopClientSock glob chans handle

isDisconnect :: Event -> Bool
isDisconnect SockClientDisconnect = True
isDisconnect _ = False

readClientSock :: Handle -> Chan Event -> IO ()
readClientSock hdl chan = do
  eof <- hIsEOF hdl
  if eof then
    writeChan chan SockClientDisconnect
  else do  
    input <- hGetLine hdl
    case parseJson input of
      Just e -> writeChan chan e
      Nothing -> putStrLn $ "LOG: Client Reading unknown msg " ++ input
    readClientSock hdl chan

parseJson :: String -> Maybe Event
parseJson json = Nothing -- TODO: parse JSON