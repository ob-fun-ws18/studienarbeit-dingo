{-|
Module      : P2PChat.Socket
Description : Socket Helpers: Reading from Handle and Converting Socket Addresses
Copyright   : (c) Chris Brammer, 2019
                  Wolfgang Gabler, 2019
-}

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

-- | Read from a handle (Exceptions are caught due to Windows throwing on EOF)
readHandle :: Handle -> IO (Maybe BL.ByteString) 
readHandle handle = readHandle' handle `catch` handler
  where
    handler :: SomeException -> IO (Maybe BL.ByteString) 
    handler ex = return Nothing

-- | Implementation to read a single line from a given handle
readHandle' :: Handle -- ^ Handle to read from
            -> IO (Maybe BL.ByteString) -- ^ Nothing on EOF, Maybe Input otherwise
readHandle' handle = do
  eof <- hIsEOF handle
  if eof then
    return Nothing
  else do
    input <- B.hGetLine handle
    return $ Just $ BL.fromStrict input

-- | Returns a String given a SockAddr
fromSockAddr :: SockAddr -- ^ SockAddr to convert
             -> (String, Int)  -- ^ Tuple containing the IP-Address and the Port
fromSockAddr (SockAddrInet  pn   ha)    = (hostAddrToString ha,  fromIntegral pn)
-- ToDo: Handle ipv6

-- | Returns a String given a HostAddress
hostAddrToString :: HostAddress  -- ^ To parse
                 -> String  -- ^ IP of HostAddress e.g "127.0.0.1"
hostAddrToString addr = intercalate "." $ map show [d1,d2,d3,d4]
 where (d1,d2,d3,d4) = hostAddressToTuple addr