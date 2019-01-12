{-# LANGUAGE OverloadedStrings #-}

module P2PChat (
  startP2PChat
, StartConfig(..)
, StartMode(..)
) where

import P2PCommon
import P2PTerm

import Network.Socket

import Data.Aeson
import Data.UUID.V1 (nextUUID)

import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

data StartMode = StartHost | StartClient String Int
data StartConfig = StartConfig {
  username :: String,
  hostPort :: Int,
  mode :: StartMode
}

data State = State String -- uuid

data Channels = Channels {
  cmain :: Chan Event,
  ccmd :: Chan Event,
  csock :: Chan Event
}

startP2PChat :: StartConfig -> IO ()
startP2PChat cfg = do
  putStrLn "startP2PChat"
  chanMain <- newChan
  chanCmdOut <- newChan
  chanSockOut <- newChan
  let chans = Channels { cmain=chanMain, ccmd=chanCmdOut, csock=chanSockOut }
  cmdId <- startCmd (cmain chans) (ccmd chans)
  handleChannels chans
  putStrLn "Ending P2PChat"
  -- case nextUUID of
  --  Nothing -> 
  --  Just uuid ->
  -- start socketmanager
  -- start cmd

handleChannels :: Channels -> IO ()
handleChannels chans = do
  event <- readChan (cmain chans)
  case event of
    (CmdInput input) -> writeChan (ccmd chans) (CmdOutput input)
    CmdQuit -> return ()
  handleChannels chans
