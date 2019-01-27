{-# LANGUAGE ScopedTypeVariables #-}
module SocketSpec (spec) where

import P2PChat.Socket
import Network.Socket
import Test.Hspec

spec :: Spec
spec = describe "Socket" $ do
    describe "Helper Function" $ do
        it "fromSockAddr" $
            fromSockAddr (SockAddrInet 1337 (tupleToHostAddress (127,0,0,1))) `shouldBe` ("127.0.0.1", 1337)