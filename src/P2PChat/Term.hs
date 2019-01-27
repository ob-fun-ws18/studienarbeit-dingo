{-|
Module      : P2PChat.Socket.Client
Description : Terminal/Console Input and Output routines
Copyright   : (c) Chris Brammer, 2019
                  Wolfgang Gabler, 2019
-}
module P2PChat.Term (
  startTerminal
) where

import P2PChat.Common
import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

-- | Starts all threads to handle the Console
startTerminal :: Channels -> IO ThreadId
startTerminal chans = do 
  forkIO $ readCmd (cterm chans)
  forkIO $ readCterm (cterm chans) (cmain chans)

-- | Forwards Input Commands to the main handling routine via channel, or writes to console
readCterm :: Chan Event -> Chan Event -> IO ()
readCterm ccmd cmain = do
  cmd <- readChan ccmd
  handle cmd
  readCterm ccmd cmain
  where 
    handle (CmdOutput s) = putStrLn s
    handle cmd = writeChan cmain cmd

-- | Reads Input from the Console
readCmd :: Chan Event -> IO ()
readCmd ccmd = do
  input <- getLine
  writeChan ccmd (handle input)
  readCmd ccmd
  where
    handle ":quit" = CmdQuit
    handle s = CmdInput s