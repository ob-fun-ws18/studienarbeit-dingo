module Main where

import           System.IO
import           Network.Socket
import           Network.Socket.ByteString     as BS
import qualified Data.ByteString.Char8         as B
import qualified Control.Exception             as E
import           Control.Monad                  ( unless
                                                , forever
                                                , void
                                                )
import           Control.Concurrent             ( forkFinally, killThread, forkIO, MVar, putMVar, threadDelay, newEmptyMVar, takeMVar)

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

portInput :: Parser Int
portInput = option
  auto
  (  long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "Port to listen on"
  <> showDefault
  <> value 4242
  <> metavar "INT"
  )

ipInput :: Parser String
ipInput = strOption
  (  long "ipaddr"
  <> short 'i'
  <> metavar "IPADDR"
  <> help "Ip to connect to"
  <> showDefault
  <> value "127.0.0.1"
  <> metavar "STRING"
  )



data ServerOption = Client | Server

data Input = Input ServerOption Int String

input :: Parser Input
input =
  Input
    <$> flag
          Server
          Client
          (long "client" <> short 'c' <> help
            "Start a client, otherwise a server"
          )
    <*> portInput
    <*> ipInput

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout NoBuffering
  main' =<< execParser opts
 where
  opts = info
    (input <**> helper)
    (fullDesc <> progDesc "P2P Chat Client" <> header
      "hello - a test for optparse-applicative"
    )


main' :: Input -> IO ()
main' (Input Client port ip) = doClient ip port
main' (Input Server port _ ) = doServer port

doClient :: String -> Int -> IO ()
doClient ipAddr port = do
  putStrLn "Started Client..."
  E.bracket (open ipAddr port) close loop
  return()
 where
  resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr : _ <- getAddrInfo (Just hints) (Just host) (Just (show port))
    return addr
  open host port = do 
    addr <- resolve host port
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock
  loop sock = do
    let actions = [beat, getLine]
    msg <- compete actions
    case msg of 
      "/quit" -> do 
        putStrLn "Quitting..."
      [] -> loop sock
      x -> do 
        BS.send sock (B.pack msg)
        loop sock
      
beat :: IO (String)
beat = do
  threadDelay 2000000
  return "Heatbeat"

compete :: [IO a] -> IO a
compete actions = do
  mvar <- newEmptyMVar
  tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
  result <- takeMVar mvar
  mapM_ killThread tids
  return result

doServer :: Int -> IO ()
doServer port = do
  putStrLn "Started Server..."
  id <- forkIO (E.bracket (open port) shutdown loop)
  waitForQuit
 where
  waitForQuit = do
    putStrLn "Type '/quit' to close the server"
    msg <- getLine
    case msg of 
      "/quit" -> do 
        putStrLn "Quitting..."
      [] -> waitForQuit    
  open port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock RecvTimeOut 10000
    setSocketOption sock SendTimeOut 10000    
    bind sock (SockAddrInet (toEnum port) iNADDR_ANY)
    listen sock 2
    return sock
  loop sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    void $ forkFinally (talk conn) (\_ -> shutdown conn)
  talk conn = do
    -- fork here to have a send "thread"
    msg <- BS.recv conn 256
    unless (B.length msg == 0) $ do
      putStrLn $ (show conn) ++ (B.unpack msg)
      talk conn
  shutdown conn = do
    putStrLn "Shutting Down!"
    close conn
