module P2PChat.Socket (
  readHandle,
  fromSockAddr
) where

import System.IO
import Network.Socket
import Control.Exception
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

readHandle :: Handle -> IO (Maybe BL.ByteString) 
readHandle handle = readHandle' handle `catch` handler
  where
    handler :: SomeException -> IO (Maybe BL.ByteString) 
    handler ex = return Nothing

readHandle' :: Handle -> IO (Maybe BL.ByteString)
readHandle' handle = do
  eof <- hIsEOF handle
  if eof then
    return Nothing
  else do
    input <- B.hGetLine handle
    return $ Just $ BL.fromStrict input

fromSockAddr :: SockAddr -> (String, Int)
fromSockAddr (SockAddrInet  pn   ha)    = (hostAddrToString ha,  fromIntegral pn)

hostAddrToString :: HostAddress -> String
hostAddrToString addr = intercalate "." $ map show [d1,d2,d3,d4]
 where (d1,d2,d3,d4) = hostAddressToTuple addr