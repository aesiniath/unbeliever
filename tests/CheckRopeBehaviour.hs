{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckRopeBehaviour where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HashMap
import qualified Data.FingerTree as F
import Data.Foldable (length)
import qualified Data.Text.Short as S
import Test.Hspec

import Core.Text.Rope

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

        it "concatonating two Ropes correctly" $ do
             ("H₂" :: Rope) <> ("SO₄" :: Rope)  `shouldBe` ("H₂SO₄" :: Rope)
