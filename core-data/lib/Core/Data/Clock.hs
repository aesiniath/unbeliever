{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
The standard package for working with dates and times in Haskell, __time__, is
/awkward/. That's a subjective judgment, but over the years there have been
few areas more frustrating than trying to do pragmatic things with calendars
and clocks. This module represents some opinionated approaches to working with
times and dates, and a place to collect some hard-won idioms for converting
between things.

Our original use was wanting to conveniently measure things happening on
distributed computer systems. Since machine clock cycles are in units of
nanoseconds, this has the nice property that, assuming the system clock is not
corrupted, two subsequent events from the same source process are likely to
have monotonically increasing timestamps. And even if the system clock goes to
hell, they're still decently likely to be unique per device. Make for good
keys.

So the timestamp type herein 'Time' is nanoseconds since the Unix epoch; which
in (signed) 64 bits means that you can represent times between early in the
morning of 21 September 1677 through just before midnight on 11 April 2262.
The primary use isn't doing calendaring, though; it's just working with
machine generated timestamps in distributed systems and for conveying start
and end times around in your program.
-}
module Core.Data.Clock (
    -- * Time type
    Time(..),
    getCurrentTimeNanoseconds,

    -- * Conversions
    Instant (fromTime, intoTime),

    -- * Internals
    unTime,
    convertFromTime,
) where

import Control.Applicative ((<|>))
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

The 'Show' instance displays the 'Time' as seconds with the nanosecond
precision expressed as a decimal amount after the interger, ie:

>>> t <- getCurrentTimeNanoseconds
>>> show t
2014-07-31T23:09:35.274387031Z

However this doesn't change the fact the underlying representation counts
nanoseconds since epoch:

>>> show $ unTime t
1406848175274387031

There is a 'Read' instance that is reasonably accommodating:

>>> read "2014-07-31T13:05:04.942089001Z" :: Time
2014-07-31T13:05:04.942089001Z

>>> read "1406811904.942089001" :: Time
2014-07-31T13:05:04.942089001Z

>>> read "1406811904" :: Time
2014-07-31T13:05:04.000000000Z

In case you're wondering, the valid range of nanoseconds that fits into the
underlying 'Int64' is:

>>> show $ minBound :: Time
1677-09-21T00:12:43.145224192Z

>>> show $ maxBound :: Time
2262-04-11T23:47:16.854775807Z

so in a quarter millenium's time, yes, you'll have the Y2262 Problem.
Haskell code from today will, of course, still be running, so in the mid
Twenty-Third century you will need to replace this implementation with
something else.

@since 0.3.3
-}
newtype Time = Time Int64
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bounded, Generic)


{- |
@since 0.3.3
-}
unTime :: Time -> Int64
unTime (Time ticks) = ticks
{-# INLINE unTime #-}

instance Show Time where
    show t = H.timePrint ISO8601_Precise (convertFromTime t)

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

instance Read Time where
    readsPrec _ s = maybeToList $ (,"") <$> parseInput s

parseInput :: String -> Maybe Time
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

    reduceDateTime :: H.DateTime -> Time
    reduceDateTime = convertToTime . H.timeGetElapsedP

{- |
Convert between different representations of time.

@since 0.3.3
-}
class Instant a where
    fromTime :: Time -> a
    intoTime :: a -> Time

instance Instant Int64 where
    fromTime = unTime
    intoTime = Time

instance Instant UTCTime where
    fromTime = posixSecondsToUTCTime . convertToPosix
    intoTime = convertFromPosix . utcTimeToPOSIXSeconds

instance Instant POSIXTime where
    fromTime = convertToPosix
    intoTime = convertFromPosix

convertFromPosix :: POSIXTime -> Time
convertFromPosix =
    let nano :: POSIXTime -> Int64
        nano = floor . (* 1000000000) . toRational
     in Time . fromIntegral . nano

convertToPosix :: Time -> POSIXTime
convertToPosix = fromRational . (/ 1e9) . fromIntegral

{- |
Get the current system time, expressed as a 'Time' (which is to
say, number of nanoseconds since the Unix epoch).

@since 0.3.3
-}
getCurrentTimeNanoseconds :: IO Time
getCurrentTimeNanoseconds = do
    p <- H.timeCurrentP
    return $! convertToTime p

--
-- We're not exposing this as an Instant instance because we want the
-- dependency on **hourglass** to go away.
--
convertToTime :: H.ElapsedP -> Time
convertToTime (H.ElapsedP (H.Elapsed (H.Seconds seconds)) (H.NanoSeconds nanoseconds)) =
    let s = fromIntegral seconds :: Int64
        ns = fromIntegral nanoseconds
     in Time $! (s * 1000000000) + ns

convertFromTime :: Time -> H.ElapsedP
convertFromTime (Time ticks) =
    let (s, ns) = divMod ticks 1000000000
     in H.ElapsedP (H.Elapsed (H.Seconds (s))) (H.NanoSeconds (ns))
