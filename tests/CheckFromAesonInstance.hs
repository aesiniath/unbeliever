{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckFromAesonInstance where

import Core.Data
import Core.Encoding.Json
import Data.Aeson
import Data.Aeson.Types
import Test.Hspec

k = JsonKey "intro"

v = JsonString "Hello"

j = JsonObject (intoMap [(k, v)])

r = JsonArray [JsonBool False, JsonNull, JsonNumber 42]

checkFromAesonInstance :: Spec
checkFromAesonInstance = do
    describe "fromAeson should transform" $ do
        it "Aeson.String to JsonString" $ do
            fromAeson (String "Hello") `shouldBe` v
        it "Aeson.Object to JsonObject" $ do
            fromAeson (object ["intro" .= String "Hello"]) `shouldBe` j
        it "JsonList to Aeson.List" $ do
            fromAeson (listValue id [Bool False, Null, Number 42]) `shouldBe` r
