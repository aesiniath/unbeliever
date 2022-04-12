{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckJsonWrapper where

import Core.Data
import Core.Encoding.Json
import Core.Text
import Test.Hspec

k = JsonKey "intro"

v = JsonString "Hello"

v2 =
    JsonString
        [quote|Joy to the "world", they sang\nIn the \"matrix\" softly|]

j = JsonObject (intoMap [(k, v)])

j2 =
    JsonObject
        ( intoMap
            [ (JsonKey "song", JsonString "Thriller")
            , (JsonKey "other", JsonString "A very long name for the \"shadow of the moon\".")
            , (JsonKey "answer", JsonNumber 42)
            , (JsonKey "pie", JsonNumber 6.62607015e-34)
            ,
                ( JsonKey "four"
                , JsonObject
                    ( intoMap
                        [ (JsonKey "n1", r)
                        ]
                    )
                )
            ]
        )

b = packBytes "{\"cost\": 4500}"

r = JsonArray [JsonBool False, JsonNull, JsonNumber 42]

checkJsonWrapper :: Spec
checkJsonWrapper = do
    describe "JsonValue encoding" $ do
        it "JSON String should be wrapped in quotes" $ do
            encodeToUTF8 v `shouldBe` packBytes "\"Hello\""

        it "JSON Strings are escaped properly" $ do
            encodeToUTF8 v2
                `shouldBe` packBytes
                    [quote|"Joy to the \"world\", they sang\\nIn the \\\"matrix\\\" softly"|]

        it "JSON Numbers differentiate between integers and floats" $ do
            encodeToUTF8 (JsonNumber 42) `shouldBe` packBytes "42"
            encodeToUTF8 (JsonNumber 3.141592) `shouldBe` packBytes "3.141592"
            encodeToUTF8 (JsonNumber 2.99792458e8) `shouldBe` packBytes "299792458"
            encodeToUTF8 (JsonNumber 6.62607015e-34) `shouldBe` packBytes "6.62607015e-34"

        it "JSON Array renders correctly" $ do
            encodeToUTF8 r `shouldBe` packBytes "[false,null,42]"

        it "JSON Object renders correctly" $ do
            encodeToUTF8 j `shouldBe` packBytes "{\"intro\":\"Hello\"}"

        it "decoding an Object parses" $ do
            decodeFromUTF8 b
                `shouldBe` Just
                    (JsonObject (intoMap [(JsonKey "cost", JsonNumber 4500)]))

        it "complex JSON Object round trips" $ do
            decodeFromUTF8 (encodeToUTF8 j2) `shouldBe` Just j2
