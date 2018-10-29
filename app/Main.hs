module Main where

import           System.IO
import           Network.Socket
import           Network.Socket.ByteString     as BS
import qualified Data.ByteString.Char8         as B

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Use 's' for server or 'c' for client"
  choice <- getLine
  main' choice

main' :: String -> IO ()
main' "s" = doServer
main' "c" = doClient
main' _   = do
  putStrLn "Use 's' for server or 'c' for client"
  choice <- getLine
  main' choice

doClient :: IO ()
doClient = do
  putStrLn "Started Client..."
  return ()

doServer :: IO ()
doServer = do
  putStrLn "Started Server..."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1           -- make socket immediately reusable - eases debugging.
  bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
  listen sock 2                              -- set a max of 2 queued connections
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock     -- accept a connection and handle it
  putStrLn "Got connection"
  runConn conn            -- run our server's logic
  mainLoop sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  BS.send sock (B.pack "Hello!\n")
  close sock

