{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckExternalizing where

import Core.Data
import Core.Encoding.External
import Core.Text
import Data.Int
import Data.Scientific
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Word
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

        it "Int64 behaves when QuickChecked" $ do
            property (prop_RoundTrip_External :: Int64 -> Bool)

        it "Word16 behaves when QuickChecked" $ do
            property (prop_RoundTrip_External :: Word16 -> Bool)

        it "Timestamps and Days" $ do
            formatExternal (intoTime (1660802416710578538 :: Int64)) `shouldBe` packRope "2022-08-18T06:00:16.710578538Z"
            parseExternal (packRope "2022-08-18T06:00:16.710578538Z") `shouldBe` Just (intoTime (1660802416710578538 :: Int64))
            formatExternal (ModifiedJulianDay 59809) `shouldBe` packRope "2022-08-18"
            parseExternal (packRope "2022-08-18") `shouldBe` Just (ModifiedJulianDay 59809)

        it "Scientific" $ do
            formatExternal (299792458 :: Scientific) `shouldBe` packRope "2.99792458e8"
            parseExternal (packRope "2.99792458e8") `shouldBe` Just (299792458 :: Scientific)

prop_RoundTrip_External :: (Externalize a, Eq a) => a -> Bool
prop_RoundTrip_External t = (parseExternal . formatExternal) t == Just t
