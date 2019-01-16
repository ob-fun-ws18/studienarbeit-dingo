module P2PTerm (
  startCmd
) where

import P2PCommon
import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

startCmd :: Chan Event -> Chan Event-> IO ThreadId
startCmd cmain cterm = do 
  forkIO $ readCmd cterm
  forkIO $ readCterm cterm cmain

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
