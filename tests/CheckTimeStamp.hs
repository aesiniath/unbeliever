{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals #-}

{-# OPTIONS -fno-warn-orphans #-}

module CheckTimeStamp where

import Core.Data.Clock
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Int (Int64)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Test.Hspec
import Test.QuickCheck

checkTimeStamp :: Spec
checkTimeStamp = do
    describe "Smallest valid Time" $
        let t = intoTime (-9223372036854775808 :: Int64)
         in do
                it "should be representable" $ do
                    t `shouldBe` (minBound :: Time)

                it "should be match when Shown" $ do
                    show t `shouldBe` show (minBound :: Time)

                it "should equal expected value" $ do
                    show t `shouldBe` "1677-09-21T00:12:43.145224192Z"

    describe "Largest valid Time" $
        let t = intoTime (9223372036854775807 :: Int64)
         in do
                it "should be representable" $ do
                    t `shouldBe` (maxBound :: Time)

                it "should be match when Shown" $ do
                    show t `shouldBe` show (maxBound :: Time)

                it "should equal expected value" $ do
                    show t `shouldBe` "2262-04-11T23:47:16.854775807Z"

    describe "Printing and parsing with precise format" $ do
        it "formats a known date correctly" $ do
            show (intoTime (1406849015948797001 :: Int64)) `shouldBe` "2014-07-31T23:23:35.948797001Z"

    describe "Round trip through Read and Show instances" $ do
        it "outputs a correctly formated ISO 8601 timestamp when Shown" $ do
            show (intoTime (1406849015948797001 :: Int64)) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (intoTime (1406849015948797001 :: Int64)) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (intoTime (0 :: Int64)) `shouldBe` "1970-01-01T00:00:00.000000000Z"

        it "Reads ISO 8601 timestamps" $ do
            read "2014-07-31T23:23:35.948797001Z" `shouldBe` intoTime (1406849015948797001 :: Int64)
            read "2014-07-31T23:23:35Z" `shouldBe` intoTime (1406849015000000000 :: Int64)
            read "2014-07-31" `shouldBe` intoTime (1406764800000000000 :: Int64)

        it "reads the Unix epoch date" $
            read "1970-01-01" `shouldBe` intoTime (0 :: Int64)

        it "permissively reads various formats" $ do
            show (read "1970-01-01T00:00:00.000000000Z" :: Time) `shouldBe` "1970-01-01T00:00:00.000000000Z"
            show (read "1970-01-01" :: Time) `shouldBe` "1970-01-01T00:00:00.000000000Z"
            show (read "0" :: Time) `shouldBe` "1970-01-01T00:00:00.000000000Z"

        it "permissively reads Posix seconds since epoch" $ do
            show (read "1406849015.948797001" :: Time) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (read "1406849015.948797" :: Time) `shouldBe` "2014-07-31T23:23:35.948797000Z"
            show (read "1406849015.948" :: Time) `shouldBe` "2014-07-31T23:23:35.948000000Z"
            show (read "1406849015" :: Time) `shouldBe` "2014-07-31T23:23:35.000000000Z"
    {-
        This is a bit fragile, depending as it does on the serialization to String
        in the Show instance of UTCTime. Not that they're going to change it
        anytime soon.
    -}

    describe "Round trip through base time types" $ do
        it "converts to POSIXTime and back again" $ do
            let t = intoTime (1406849015948797001 :: Int64)
            intoTime (fromTime t :: POSIXTime) `shouldBe` t
            show (fromTime t :: POSIXTime) `shouldBe` "1406849015.948797001s"

        it "converts to UTCTime and back again" $ do
            let t = intoTime (1406849015948797001 :: Int64)
            intoTime (fromTime t :: UTCTime) `shouldBe` t
            show (fromTime t :: UTCTime) `shouldBe` "2014-07-31 23:23:35.948797001 UTC"

        it "behaves when QuickChecked" $ do
            property prop_RoundTrip_ReadShow

        it "converts to Day and back again" $ do
            let t = intoTime (1406849015948797001 :: Int64)
            let d = fromTime t :: Day
            show (intoTime d) `shouldBe` "2014-07-31T00:00:00.000000000Z"

    describe "Round trip via JSON" $ do
        it "explicit JSON encoding is correct" $ do
            let t = read "2018-05-01T01:42:12Z" :: Time
            encode t `shouldBe` L.pack "\"2018-05-01T01:42:12.000000000Z\""

        it "converts to a JSON String and back again" $ do
            let t = read "2018-05-01T01:42:12Z" :: Time
            (decode . encode) t `shouldBe` Just t

        it "behaves when QuickChecked" $ do
            property prop_RoundTrip_Aeson

instance Arbitrary Time where
    arbitrary = do
        tick <- arbitrary
        return (intoTime (tick :: Int64))

prop_RoundTrip_ReadShow :: Time -> Bool
prop_RoundTrip_ReadShow t = (read . show) t == t

prop_RoundTrip_Aeson :: Time -> Bool
prop_RoundTrip_Aeson t = (decode . encode) t == Just t
