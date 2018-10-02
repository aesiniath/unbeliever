{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckRopeBehaviour where

import qualified Data.FingerTree as F
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

        it "QuasiQuoted string literal is IsString" $ do
            [quote|Hello|] `shouldBe` ("Hello" :: String)
            [quote|Hello|] `shouldBe` ("Hello" :: Rope)

        it "handles multi-line string literals" $ do
            [quote|
Hello
            |] `shouldBe` ("Hello\n" :: Rope)

