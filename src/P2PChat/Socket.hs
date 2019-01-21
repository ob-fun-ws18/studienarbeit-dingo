module P2PChat.Socket (
  readHandle
) where

import System.IO
import Control.Exception
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
