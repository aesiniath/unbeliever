{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckExternalizing where

import Core.Encoding.External
import Core.Text
import Data.Int
import Data.Scientific
import Test.Hspec
import Test.QuickCheck (property)

checkExternalizing :: Spec
checkExternalizing = do
    describe "Values can be externalized" $ do
        it "Int, Int32, Int64" $ do
            formatExternal (42 :: Int) `shouldBe` packRope "42"
            parseExternal (packRope "42") `shouldBe` Just (42 :: Int)
            formatExternal (299792458 :: Int32) `shouldBe` packRope "299792458"
            parseExternal (packRope "299792458") `shouldBe` Just (299792458 :: Int32)
            formatExternal (9223372036854775807 :: Int64) `shouldBe` packRope "9223372036854775807"
            parseExternal (packRope "9223372036854775807") `shouldBe` Just (9223372036854775807 :: Int64)

        it "behaves when QuickChecked" $ do
            property (prop_RoundTrip_External :: Int64 -> Bool)

        it "Scientific" $ do
            formatExternal (299792458 :: Scientific) `shouldBe` packRope "2.99792458e8"
            parseExternal (packRope "2.99792458e8") `shouldBe` Just (299792458 :: Scientific)


prop_RoundTrip_External :: (Externalize a, Eq a) => a -> Bool
prop_RoundTrip_External t = (parseExternal . formatExternal) t == Just t
