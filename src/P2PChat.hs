{-|
Module      : P2PChat
Description : Main Entry Point for the P2PChat, Handles Host/Client and Migration
Copyright   : (c) Chris Brammer, 2019
                  Wolfgang Gabler, 2019
-}
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

-- | Generates a random UUID to identify the client/host and sets up 3 channels for communication between threads
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

-- | Starts either the Client or the Host with given Settings and Channels
startP2PChat' :: StartMode -> Channels -> Global -> IO ()
startP2PChat' StartHost chans glob = startHost chans glob
startP2PChat' (StartClient ip port) chans glob = startClient chans glob ip port

-- | Starts the Host
startHost :: Channels -> Global -> IO ()
startHost chans glob = do
  sockId <- startSocketHost glob chans
  doHost glob chans sockId

-- | Starts the client
startClient :: Channels -> Global -> String -> Int -> IO ()  
startClient chans glob host port = do
  sockId <- startSocketClient host port glob chans
  case sockId of
    Just id -> doClient glob chans id []
    Nothing -> return ()

-- | Loop for the Host
doHost :: Global -> Channels -> ThreadId -> IO ()
doHost glob chans sockId = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> do 
      writeChan (csock chans) (SockMsgOut input)
      writeChan (cterm chans) (CmdOutput (myUserName glob ++ ": " ++ input))
    CmdQuit -> killThread sockId
    (SockHostConnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User joined: " ++ mUsername user ++ " -> " ++ show members))
    (SockHostDisconnect user members) ->
      writeChan (cterm chans) (CmdOutput ("User left: " ++ mUsername user  ++ " -> " ++ show members))
    (SockMsgIn user msg) -> writeChan (cterm chans) (CmdOutput (">>>>> " ++ user ++ ": " ++ msg))
  unless (isQuit event) (doHost glob chans sockId)

-- | Loop for the Client
doClient :: Global -> Channels -> ThreadId -> [Member] -> IO ()
doClient glob chans sockId ms = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> do 
      writeChan (csock chans) (SockMsgOut input)
      unless (isQuit event) (doClient glob chans sockId ms)
    CmdQuit -> do 
      killThread sockId
      unless (isQuit event) (doClient glob chans sockId ms)
    (SockHostConnect user members) -> do
      writeChan (cterm chans) (CmdOutput ("User joined: " ++ mUsername user  ++ " -> " ++ show members))
      unless (isQuit event) (doClient glob chans sockId members)
    (SockHostDisconnect user members) -> do
      writeChan (cterm chans) (CmdOutput ("User left: " ++ mUsername user  ++ " -> " ++ show members))
      unless (isQuit event) (doClient glob chans sockId members)
    (SockMsgIn user msg) -> do 
      writeChan (cterm chans) (CmdOutput (">>>>> " ++ user ++ ": " ++ msg))
      unless (isQuit event) (doClient glob chans sockId ms)
    SockClientDisconnect -> handleClientMigration glob chans sockId ms

-- | Routine to migrate as a client to a new host, or transforming into the new host
handleClientMigration :: Global   -- ^ Global State
                      -> Channels -- ^ Channels for communication
                      -> ThreadId -- ^ The current ThreadId of the running client
                      -> [Member] -- ^ List of all connected Clients (ordered)
                      -> IO()
handleClientMigration glob chans sockId [m] = putStrLn "Disconnect (only us left, close down)"
handleClientMigration glob chans sockId (m:ms) = do 
  killThread sockId -- kill current client
  let newHostname = mHostname m
      newPort = mPort m
  if myUUID glob == mUUID m
    then do -- We are to be Host
      putStrLn $ "Migrating to host on port: " ++ show newPort
      startHost chans (Global (myUserName glob) (myUUID glob) newPort)
    else do -- Migrate to new Host
      threadDelay 1000000 --dont connect immediately, host needs to migrate!      
      putStrLn $ "Connecting to new Host: " ++ concat [newHostname, ":", show newPort]
      startClient chans glob newHostname newPort

