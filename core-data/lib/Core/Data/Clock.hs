{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Core.Data.Clock (
    -- * Time type
    TimeStamp,
    getCurrentTimeNanoseconds,

    -- * Conversions
    Instant (fromTime, intoTime),
    unTimeStamp,
    -- silent
    convertFromTimeStamp,
) where

import Control.Applicative ((<|>))
import Core.Text.Bytes (Bytes)
import Core.Text.Rope (Rope, Textual (..))
import Data.Bifoldable (Bifoldable)
import Data.ByteString qualified as B (ByteString)
import Data.Hourglass qualified as H (
    DateTime (..),
    Elapsed (..),
    ElapsedP (..),
    ISO8601_Date (..),
    ISO8601_DateAndTime (..),
    NanoSeconds (..),
    Seconds (..),
    TimeFormat (..),
    TimeFormatElem (..),
    TimeFormatString (..),
    Timeable (timeGetElapsedP),
    timeParse,
    timePrint,
 )
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (
    POSIXTime,
    posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
 )
import GHC.Generics
import Time.System qualified as H (
    timeCurrentP,
 )

{- |
Number of nanoseconds since the Unix epoch.

The 'Show' instance displays the 'TimeStamp' as seconds with the nanosecond
precision expressed as a decimal amount after the interger, ie:

>>> t <- getCurrentTimeNanoseconds
>>> show t
2014-07-31T23:09:35.274387031Z

However this doesn't change the fact the underlying representation counts
nanoseconds since epoch:

>>> show $ unTimeStamp t
1406848175274387031

There is a 'Read' instance that is reasonably accommodating:

>>> read "2014-07-31T13:05:04.942089001Z" :: TimeStamp
2014-07-31T13:05:04.942089001Z

>>> read "1406811904.942089001" :: TimeStamp
2014-07-31T13:05:04.942089001Z

>>> read "1406811904" :: TimeStamp
2014-07-31T13:05:04.000000000Z

In case you're wondering, the valid range of nanoseconds that fits into the
underlying 'Int64' is:

>>> show $ minBound :: TimeStamp
1677-09-21T00:12:43.145224192Z

>>> show $ maxBound :: TimeStamp
2262-04-11T23:47:16.854775807Z

so in a quarter millenium's time, yes, you'll have the Y2262 Problem.
Haskell code from today will, of course, still be running, so in the mid
Twenty-Third century you will need to replace this implementation with
something else.

@since 0.3.3
-}
newtype TimeStamp = TimeStamp
    { unTimeStamp :: Int64
    }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bounded, Generic)

instance Show TimeStamp where
    show t = H.timePrint ISO8601_Precise (convertFromTimeStamp t)

{- |
Format string describing full (nanosecond) precision ISO8601 time,

> 2014-07-31T23:09:35.274387019Z
-}
data ISO8601_Precise = ISO8601_Precise

instance H.TimeFormat ISO8601_Precise where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_Year
            , H.Format_Text '-'
            , H.Format_Month2
            , H.Format_Text '-'
            , H.Format_Day2
            , H.Format_Text 'T'
            , H.Format_Hour
            , H.Format_Text ':'
            , H.Format_Minute
            , H.Format_Text ':'
            , H.Format_Second
            , H.Format_Text '.'
            , H.Format_Precision 9
            , H.Format_Text 'Z'
            ]

data ISO8601_Seconds = ISO8601_Seconds

instance H.TimeFormat ISO8601_Seconds where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_Year
            , H.Format_Text '-'
            , H.Format_Month2
            , H.Format_Text '-'
            , H.Format_Day2
            , H.Format_Text 'T'
            , H.Format_Hour
            , H.Format_Text ':'
            , H.Format_Minute
            , H.Format_Text ':'
            , H.Format_Second
            , H.Format_Text 'Z'
            ]

data Posix_Precise = Posix_Precise

instance H.TimeFormat Posix_Precise where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_UnixSecond
            , H.Format_Text '.'
            , H.Format_MilliSecond
            , H.Format_MicroSecond
            , H.Format_NanoSecond
            ]

data Posix_Micro = Posix_Micro

instance H.TimeFormat Posix_Micro where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_UnixSecond
            , H.Format_Text '.'
            , H.Format_MilliSecond
            , H.Format_MicroSecond
            ]

data Posix_Milli = Posix_Milli

instance H.TimeFormat Posix_Milli where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_UnixSecond
            , H.Format_Text '.'
            , H.Format_MilliSecond
            ]

data Posix_Seconds = Posix_Seconds

instance H.TimeFormat Posix_Seconds where
    toFormat _ =
        H.TimeFormatString
            [ H.Format_UnixSecond
            ]

instance Read TimeStamp where
    readsPrec _ s = maybeToList $ (,"") <$> parseInput s

parseInput :: String -> Maybe TimeStamp
parseInput = fmap reduceDateTime . parse
  where
    parse :: String -> Maybe H.DateTime
    parse x =
        H.timeParse ISO8601_Precise x
            <|> H.timeParse ISO8601_Seconds x
            <|> H.timeParse H.ISO8601_DateAndTime x
            <|> H.timeParse H.ISO8601_Date x
            <|> H.timeParse Posix_Precise x
            <|> H.timeParse Posix_Micro x
            <|> H.timeParse Posix_Milli x
            <|> H.timeParse Posix_Seconds x

    reduceDateTime :: H.DateTime -> TimeStamp
    reduceDateTime = convertToTimeStamp . H.timeGetElapsedP

{- |
Convert between different representations of time.

@since 0.3.3
-}
class Instant a where
    fromTime :: TimeStamp -> a
    intoTime :: a -> TimeStamp

instance Instant Int64 where
    fromTime = unTimeStamp
    intoTime = TimeStamp

instance Instant UTCTime where
    fromTime = posixSecondsToUTCTime . convertToPosix
    intoTime = convertFromPosix . utcTimeToPOSIXSeconds

instance Instant POSIXTime where
    fromTime = convertToPosix
    intoTime = convertFromPosix

convertFromPosix :: POSIXTime -> TimeStamp
convertFromPosix =
    let nano :: POSIXTime -> Int64
        nano = floor . (* 1000000000) . toRational
     in TimeStamp . fromIntegral . nano

convertToPosix :: TimeStamp -> POSIXTime
convertToPosix = fromRational . (/ 1e9) . fromIntegral

{- |
Get the current system time, expressed as a 'TimeStamp' (which is to
say, number of nanoseconds since the Unix epoch).

@since 0.3.3
-}
getCurrentTimeNanoseconds :: IO TimeStamp
getCurrentTimeNanoseconds = do
    p <- H.timeCurrentP
    return $! convertToTimeStamp p

--
-- We're not exposing this as an Instant instance because we want the
-- dependency on **hourglass** to go away.
--
convertToTimeStamp :: H.ElapsedP -> TimeStamp
convertToTimeStamp (H.ElapsedP (H.Elapsed (H.Seconds seconds)) (H.NanoSeconds nanoseconds)) =
    let s = fromIntegral seconds :: Int64
        ns = fromIntegral nanoseconds
     in TimeStamp $! (s * 1000000000) + ns

convertFromTimeStamp :: TimeStamp -> H.ElapsedP
convertFromTimeStamp (TimeStamp ticks) =
    let (s, ns) = divMod ticks 1000000000
     in H.ElapsedP (H.Elapsed (H.Seconds (s))) (H.NanoSeconds (ns))
