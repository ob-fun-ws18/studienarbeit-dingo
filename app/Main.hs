module Main where

import P2PChat
  
import System.IO
import Network.Socket

import qualified Data.Text as T

-- Option Parser
import Options.Applicative
import Data.Semigroup ( (<>) )
  
data Args = Args
  { username :: String
  , hostport :: Int
  , connectTo :: Maybe String
  } deriving (Show)

args :: Parser Args
args = Args
  <$> strOption (short 'u' <> long "username")
  <*> option auto (short 'p' <> long "port" <> value 4242)
  <*> (optional $ strOption $ short 'c' <> long "connect")
  
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
  
main' :: Args -> IO ()
main' (Args username port (Just connect)) = do
  -- change this to use with regex and assert format
  let ipPort = T.splitOn (T.pack ":")  (T.pack connect)
  let ip = T.unpack $ ipPort !! 0
  let port = read $ T.unpack (ipPort !! 1) :: Int
  startP2PChat $ StartConfig username port (StartClient ip port)
main' (Args username port Nothing) = startP2PChat $ StartConfig username port StartHost