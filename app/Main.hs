module Main where

import P2PChat
  
import System.IO
import Network.Socket

import qualified Data.Text as T

-- Option Parser
import Options.Applicative
import Data.Semigroup ( (<>) )

-- | Custom Data Structure for the parsed Command-Line
data Args = Args
  { username :: String
  , hostport :: Int
  , connectTo :: Maybe String
  } deriving (Show)

-- | Command-Line Parser with Options.Applicative
args :: Parser Args
args = Args
  <$> strOption (short 'u' <> long "username")
  <*> option auto (short 'p' <> long "server-port" <> value 4242)
  <*> optional (strOption $ short 'c' <> long "connect")

-- | Entry Point: Parse CMD-Line Options and set handle buffering
main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout NoBuffering
  main' =<< execParser opts
  where
  opts = info
    (args <**> helper)
    (fullDesc <> progDesc "P2P Chat Client" <> header
      "hello - a test for optparse-applicative"
    )

-- | Unpacks and Command-Args
main' :: Args -> IO ()
main' (Args username port (Just connect)) = do
  -- change this to use with regex and assert format
  let ipPort = T.splitOn (T.pack ":")  (T.pack connect)
  let ip = T.unpack $ head ipPort
  let port = read $ T.unpack (ipPort !! 1) :: Int
  startP2PChat $ StartConfig username port (StartClient ip port)
main' (Args username port Nothing) = startP2PChat $ StartConfig username port StartHost
