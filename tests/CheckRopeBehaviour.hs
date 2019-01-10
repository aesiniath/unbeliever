{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckRopeBehaviour where

import qualified Data.FingerTree as F
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as U
import qualified Data.Text.Short as S
import Test.Hspec

import Core.Text.Rope
import Core.Text.Utilities

hydrogen = "H₂" :: Rope
sulfate = "SO₄" :: Rope

sulfuric_acid = hydrogen <> sulfate

compound = "3" <> "-" <> "ethyl" <> "-" <> "4" <> "-" <> "methyl" <> "hexane" :: Rope

checkRopeBehaviour :: Spec
checkRopeBehaviour = do
    describe "Rope data type" $
      do
        it "IsString instance behaves" $ do
            unRope ("Hello" :: Rope) `shouldBe` F.singleton (S.pack "Hello")

        it "calculates length accurately" $ do
            width hydrogen `shouldBe` 2
            width sulfate `shouldBe` 3
            width (hydrogen <> sulfate) `shouldBe` 5

        it "Eq instance behaves" $ do
             ("" :: Rope) == ("" :: Rope) `shouldBe` True
             ("C" :: Rope) /= ("" :: Rope) `shouldBe` True
             ("" :: Rope) /= ("F" :: Rope) `shouldBe` True
             ("O" :: Rope) == ("O" :: Rope) `shouldBe` True
             ("H₂" :: Rope) == ("H₂" :: Rope) `shouldBe` True
             ("H₂" :: Rope) /= ("SO₄" :: Rope)  `shouldBe` True

        -- depended on Textual instance for String being fixed and
        -- the Eq instance being customized to ignore tree structure
        it "concatonates two Ropes correctly (Monoid)" $ do
             ("H₂" :: Rope) <> ("SO₄" :: Rope)  `shouldBe` ("H₂SO₄" :: Rope)

        it "concatonates two Ropes correctly (Textual)" $ do
             append ("SO₄" :: Rope) ("H₂" :: Rope) `shouldBe` ("H₂SO₄" :: Rope)

        it "exports to ByteString" $
          let
            expected = T.encodeUtf8 (T.pack "H₂SO₄")
          in do
            fromRope sulfuric_acid `shouldBe` expected

        it "exports to Text (Strict)" $ do
            fromRope sulfuric_acid `shouldBe` T.pack "H₂SO₄"

        it "exports to Text (Lazy)" $ do
            fromRope sulfuric_acid `shouldBe` U.pack "H₂SO₄"

        it "does the splits" $ do
            -- compare behaviour on Haskell lists
            List.splitAt 0 ("123456789" :: String) `shouldBe` ("", "123456789")
            List.splitAt 3 ("123456789" :: String) `shouldBe` ("123", "456789")

            -- expect same behaviour of Rope
            split 0 ("123456789" :: Rope) `shouldBe` ("", "123456789")
            split 3 ("123456789" :: Rope) `shouldBe` ("123", "456789")
            split 9 ("123456789" :: Rope) `shouldBe` ("123456789","")
            split 10 ("123456789" :: Rope) `shouldBe` ("123456789","")
            split (-1) ("123456789" :: Rope) `shouldBe` ("", "123456789")

            -- exercise splitting at and between piece boundaries
            split 0 compound `shouldBe` ("", "3-ethyl-4-methylhexane")
            split 1 compound `shouldBe` ("3", "-ethyl-4-methylhexane")
            split 2 compound `shouldBe` ("3-", "ethyl-4-methylhexane")
            split 4 compound `shouldBe` ("3-et", "hyl-4-methylhexane")
            --                             1234567890
            split 10 compound `shouldBe` ("3-ethyl-4-", "methylhexane")
            split 11 compound `shouldBe` ("3-ethyl-4-m", "ethylhexane")
            split 16 compound `shouldBe` ("3-ethyl-4-methyl", "hexane")
            split 21 compound `shouldBe` ("3-ethyl-4-methylhexan", "e")
            width compound `shouldBe` 22
            split 22 compound `shouldBe` ("3-ethyl-4-methylhexane", "")
            split 23 compound `shouldBe` ("3-ethyl-4-methylhexane", "")
            split (-1) compound `shouldBe` ("", "3-ethyl-4-methylhexane")

        it "does insertion correctly" $ do
            insert 3 "two" "onethree" `shouldBe` "onetwothree"
            insert 3 "Con" "Def 1" `shouldBe` "DefCon 1"
            insert 0 "one" "twothree" `shouldBe` "onetwothree"
            insert 6 "three" "onetwo" `shouldBe` "onetwothree"

    describe "QuasiQuoted string literals" $
      do
        it "string literal is IsString" $ do
            [quote|Hello|] `shouldBe` ("Hello" :: String)
            [quote|Hello|] `shouldBe` ("Hello" :: Rope)

        it "trims multi-line string literal" $ do
            [quote|
Hello
            |] `shouldBe` ("Hello\n" :: Rope)
            [quote|
Hello
World
            |] `shouldBe` ("Hello\nWorld\n" :: Rope)

    describe "Splitting into words" $ do
        it "single piece" $
          let
            text = "This is a test"
          in do
            pieces text `shouldBe` ["This","is","a","test"]

        it "single piece, long run of whitespace" $
          let
            text = "This is\na    test"
          in do
            pieces text `shouldBe` ["This","is","a","test"]

