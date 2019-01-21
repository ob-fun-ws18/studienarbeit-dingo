module P2PChat.Term (
  startTerminal
) where

import P2PChat.Common
import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

startTerminal :: Channels -> IO ThreadId
startTerminal chans = do 
  forkIO $ readCmd (cterm chans)
  forkIO $ readCterm (cterm chans) (cmain chans)

readCterm :: Chan Event -> Chan Event -> IO ()
readCterm ccmd cmain = do
  cmd <- readChan ccmd
  handle cmd
  readCterm ccmd cmain
  where 
    handle (CmdOutput s) = putStrLn s
    handle cmd = writeChan cmain cmd

readCmd :: Chan Event -> IO ()
readCmd ccmd = do
  input <- getLine
  writeChan ccmd (handle input)
  readCmd ccmd
  where
    handle ":quit" = CmdQuit
    handle s = CmdInput s