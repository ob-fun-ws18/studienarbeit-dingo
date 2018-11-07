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
import           Control.Concurrent             ( forkFinally )

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
  bla =<< execParser opts
 where
  opts = info
    (input <**> helper)
    (fullDesc <> progDesc "P2P Chat Client" <> header
      "hello - a test for optparse-applicative"
    )


bla :: Input -> IO ()
bla (Input Client port ip) = doClient ip port
bla (Input Server port _ ) = doServer port

doClient :: String -> Int -> IO ()
doClient ipAddr port = do
  putStrLn "Started Client..."
  addr <- resolve ipAddr port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  BS.send sock (B.pack "This is a test\n")
  BS.send sock (B.pack "This is a test\n")
  close sock
 where
  resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr : _ <- getAddrInfo (Just hints) (Just host) (Just (show port))
    return addr

doServer :: Int -> IO ()
doServer port = do
  putStrLn "Started Server..."
  E.bracket (open port) close loop
 where
  open port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (toEnum port) iNADDR_ANY)
    listen sock 2
    return sock
  loop sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    void $ forkFinally (talk conn) (\_ -> close conn)
  talk conn = do
    -- fork here to have a send "thread"
    msg <- BS.recv conn 1024
    unless (B.length msg == 0) $ do
      putStrLn $ B.unpack msg
      talk conn
