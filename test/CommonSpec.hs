{-# LANGUAGE ScopedTypeVariables #-}
module CommonSpec (spec) where

import P2PChat.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Test.Hspec

spec :: Spec
spec = describe "Common" $ do
    describe "isDisconnect" $ do
        it "detects Disconnect correctly" $
            isDisconnect SockClientDisconnect `shouldBe` True
        it "detects non-Disconnect correctly" $
            isDisconnect CmdQuit `shouldBe` False
    describe "isQuit" $ do
        it "detects Quit correctly" $
            isQuit CmdQuit `shouldBe` True
        it "detects non-Quit correctly" $
            isQuit (CmdOutput "test") `shouldBe` False
    describe "jsonParse" $ do
        it "jsonParse fail on no json" $
            jsonParse (B.pack "OK") `shouldBe` Nothing
        it "jsonParse correct shouldNotBe Noting" $
            jsonParse (B.pack validJson) `shouldNotBe` Nothing
        it "jsonParse OK" $
            jsonParse (B.pack validJson) `shouldBe` (Just jsonOK)
    describe "Common Helper Functions" $ do
        it "jsonEmpty" $
            jsonEmpty "test123" `shouldBe` (JsonMessage "test123" Nothing Nothing Nothing Nothing)
        it "isJsonOK" $
            isJsonOK jsonOK `shouldBe` True
        it "jsonHeartbeat" $
            jsonHeartbeat `shouldBe` (JsonMessage "Heartbeat" Nothing Nothing Nothing Nothing)
        it "jsonConnect" $
            jsonConnect "name" "uuid" 23 `shouldBe` (JsonMessage "connect" (Just (JsonPayloadConnect "name" "uuid" 23)) Nothing Nothing Nothing)
        it "jsonMessageSend" $
            jsonMessageSend "name" "uuid" `shouldBe` (JsonMessage "message" Nothing Nothing Nothing (Just $ JsonPayloadMessage "name" "uuid"))
    where
        validJson = "{\"jsClientConnected\":null,\"jsType\":\"OK\",\"jsClientDisconnected\":null,\"jsConnect\":null,\"jsMessage\":null}"
        