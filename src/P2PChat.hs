{-# LANGUAGE OverloadedStrings #-}

module P2PChat (
  startP2PChat
, StartConfig(..)
, StartMode(..)
) where

import P2PCommon
import P2PTerm
import P2PSockClient
import P2PSockHost

import Network.Socket

import Data.UUID
import Data.UUID.V4 as UUID

import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad

startP2PChat :: StartConfig -> IO ()
startP2PChat cfg = do
  uuid <- toString <$> UUID.nextRandom
  let globals = Global (username cfg) uuid (hostPort cfg)
  chanMain <- newChan
  chanCmdOut <- newChan
  chanSockOut <- newChan
  let chans = Channels { cmain=chanMain, ccmd=chanCmdOut, csock=chanSockOut }
  cmdId <- startCmd (cmain chans) (ccmd chans)
  startP2PChat' (mode cfg) chans globals
  killThread cmdId
  putStrLn "Ending P2PChat"

startP2PChat' :: StartMode -> Channels -> Global -> IO ()
startP2PChat' StartHost chans glob = startHost chans glob
startP2PChat' (StartClient ip port) chans glob = startClient chans glob ip port

startHost :: Channels -> Global -> IO ()
startHost chans glob = do
  sockId <- startSockHost glob chans
  -- TODO: Error handling
  doHost glob chans sockId
  killThread sockId

startClient :: Channels -> Global -> String -> Int -> IO ()  
startClient chans glob host port = do
  sockId <- startSockClient host port glob chans
  -- TODO: needs error handling
  doClient glob chans sockId
  killThread sockId

doHost :: Global -> Channels -> ThreadId -> IO ()
doHost glob chans sockId = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> do 
      writeChan (csock chans) (SockMsgOut input)
      writeChan (ccmd chans) (CmdOutput ((myUserName glob) ++ ": " ++ input))
    CmdQuit -> putStrLn "LOG: Should exit program here."
    (SockHostConnect user members) -> writeChan (ccmd chans) (CmdOutput ("User joined: " ++ user))
    (SockHostDisconnect user members) -> writeChan (ccmd chans) (CmdOutput ("User left: " ++ user))
    (SockMsgIn user msg) -> writeChan (ccmd chans) (CmdOutput (user ++ ": " ++ msg))
  unless (isQuit event) (doHost glob chans sockId)

doClient :: Global -> Channels -> ThreadId -> IO ()
doClient glob chans sockId = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> writeChan (csock chans) (SockMsgOut input)
    CmdQuit -> putStrLn "LOG: Should exit program here."
    (SockHostConnect user members) -> writeChan (ccmd chans) (CmdOutput ("User joined: " ++ user))
    (SockHostDisconnect user members) -> writeChan (ccmd chans) (CmdOutput ("User left: " ++ user))
    (SockMsgIn user msg) -> writeChan (ccmd chans) (CmdOutput (user ++ ": " ++ msg))
    SockClientDisconnect -> return ()
  unless (isQuit event) (doClient glob chans sockId)

isQuit :: Event -> Bool
isQuit CmdQuit = True
isQuit _ = False