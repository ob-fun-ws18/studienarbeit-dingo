{-# LANGUAGE OverloadedStrings #-}

module P2PChat (
  startP2PChat
, StartConfig(..)
, StartMode(..)
) where

import P2PChat.Common
import P2PChat.Term
import P2PChat.Socket.Client
import P2PChat.Socket.Host

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
  let chans = Channels { cmain=chanMain, cterm=chanCmdOut, csock=chanSockOut }
  cmdId <- startTerminal chans
  startP2PChat' (mode cfg) chans globals
  killThread cmdId
  putStrLn "Ending P2PChat"

startP2PChat' :: StartMode -> Channels -> Global -> IO ()
startP2PChat' StartHost chans glob = startHost chans glob
startP2PChat' (StartClient ip port) chans glob = startClient chans glob ip port

startHost :: Channels -> Global -> IO ()
startHost chans glob = do
  sockId <- startSocketHost glob chans
  -- TODO: Error handling
  doHost glob chans sockId

startClient :: Channels -> Global -> String -> Int -> IO ()  
startClient chans glob host port = do
  sockId <- startSocketClient host port glob chans
  -- TODO: needs error handling
  case sockId of
    Just id -> doClient glob chans id
    Nothing -> return ()

doHost :: Global -> Channels -> ThreadId -> IO ()
doHost glob chans sockId = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> do 
      writeChan (csock chans) (SockMsgOut input)
      writeChan (cterm chans) (CmdOutput ((myUserName glob) ++ ": " ++ input))
    CmdQuit -> killThread sockId
    (SockHostConnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User joined: " ++ (mUsername user) ++ " -> " ++ show members))
    (SockHostDisconnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User left: " ++ (mUsername user)  ++ " -> " ++ show members))
    (SockMsgIn user msg) -> writeChan (cterm chans) (CmdOutput (">>>>> " ++ user ++ ": " ++ msg))
  unless (isQuit event) (doHost glob chans sockId)

doClient :: Global -> Channels -> ThreadId -> IO ()
doClient glob chans sockId = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> writeChan (csock chans) (SockMsgOut input)
    CmdQuit -> killThread sockId
    (SockHostConnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User joined: " ++ (mUsername user)  ++ " -> " ++ show members))
    (SockHostDisconnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User left: " ++ (mUsername user)  ++ " -> " ++ show members))
    (SockMsgIn user msg) -> writeChan (cterm chans) (CmdOutput (">>>>> " ++ user ++ ": " ++ msg))
    SockClientDisconnect -> putStrLn "Disconnect"
  unless (isQuit event) (doClient glob chans sockId)
