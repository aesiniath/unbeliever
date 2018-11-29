{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckJsonWrapper where

import qualified Data.ByteString.Char8 as C
import Test.Hspec

import Core.Data
import Core.Text
import Core.Encoding.Json

k = JsonKey "intro"
v = JsonString "Hello"

j = JsonObject (fromList1 [(k, v)])

j2 = JsonObject (fromList1
        [ (JsonKey "song", JsonString "Thriller")
        , (JsonKey "other", JsonString "A very long name for the \"shadow of the moon\".")
        , (JsonKey "four", JsonObject (fromList1
                [ (JsonKey "n1", r)
                ]))
        ])

b = intoBytes (C.pack "{\"cost\": 4500}")

r = JsonArray [JsonBool False, JsonNull, JsonNumber 42]


checkJsonWrapper :: Spec
checkJsonWrapper = do
    describe "JsonValue encoding" $
      do
        it "JSON String should be wrapped in quotes" $ do
            encodeToUTF8 v `shouldBe` intoBytes (C.pack "\"Hello\"")

        it "JSON Array renders correctly" $ do
            encodeToUTF8 r `shouldBe` intoBytes (C.pack "[false,null,42]")

        it "JSON Object renders correctly" $ do
            encodeToUTF8 j `shouldBe` intoBytes (C.pack "{\"intro\":\"Hello\"}")

        it "decoding an Object parses" $ do
            decodeFromUTF8 b `shouldBe` Just (JsonObject (fromList1 [(JsonKey "cost", JsonNumber 4500)]))

        it "complex JSON Object round trips" $ do
            decodeFromUTF8 (encodeToUTF8 j2) `shouldBe` Just j2
