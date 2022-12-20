{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
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

There are quite a few other time formats around the Haskell ecosystem. You can
use the 'fromTime' and 'intoTime' methods of the 'Instant' typeclass  to
convert from one to another if you need to.
-}
module Core.Data.Clock
    ( -- * Time type
      Time
    , getCurrentTimeNanoseconds

      -- * Conversions
    , Instant (fromTime, intoTime)

      -- * Internals
    , unTime
    , epochTime
    ) where

import Control.Applicative ((<|>))
import Core.Data.Format
import Core.Text.Rope
import Data.Aeson qualified as Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson.Encoding qualified as Aeson (string)
import Data.Aeson.Types qualified as Aeson (typeMismatch)
import Data.Hourglass qualified as H
    ( DateTime (..)
    , Elapsed (..)
    , ElapsedP (..)
    , ISO8601_Date (..)
    , ISO8601_DateAndTime (..)
    , NanoSeconds (..)
    , Seconds (..)
    , Timeable (timeGetElapsedP)
    , timeParse
    , timePrint
    )
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (UTCTime, utctDay, utctDayTime))
import Data.Time.Clock.POSIX
    ( POSIXTime
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )
import GHC.Generics
import Time.System qualified as H
    ( timeCurrentP
    )

{- |
Number of nanoseconds since the Unix epoch.

The 'Show' and 'Core.Encoding.External.Externalize' instances display the
'Time' as seconds with the nanosecond precision expressed as a decimal amount
after the interger, ie:

>>> t <- getCurrentTimeNanoseconds
>>> formatExternal t
"2014-07-31T23:09:35.274387031Z"

However this doesn't change the fact the underlying representation counts
nanoseconds since epoch:

>>> show $ unTime t
"1406848175274387031"

There is a 'Externalize' instance that is reasonably accommodating:

>>> parseExternal "2014-07-31T13:05:04.942089001Z" :: Maybe Time
Just 2014-07-31T13:05:04.942089001Z

>>> parseExternal "1406811904.942089001" :: Maybe Time
Just 2014-07-31T13:05:04.942089001Z

>>> parseExternal "1406811904" :: Maybe Time
Just 2014-07-31T13:05:04.000000000Z

In case you're wondering, the valid range of nanoseconds that fits into the
underlying 'Int64' is:

>>> formatExternal (minBound :: Time)
"1677-09-21T00:12:43.145224192Z"

>>> formatExternal (maxBound :: Time)
"2262-04-11T23:47:16.854775807Z"

so in a quarter millenium's time, yes, you'll have the Y2262 Problem.
Haskell code from today will, of course, still be running, so in the mid
Twenty-Third century you will need to replace this implementation with
something else.

@since 0.3.3
-}
newtype Time = Time Int64
    deriving (Eq, Ord, Enum, Bounded, Generic)

{- |
If you need to manipulate the date or calculate elapsed time then you can
dig out the underlying 'Int64' here. We have /not/ provided instances of
'Num', 'Real', or 'Integral' for the timestamp type because adding two
timestamps doesn't really make sense. You can use 'intoTime' to reconstruct a
timestamp subsequently if necessary.

@since 0.3.3
-}
unTime :: Time -> Int64
unTime (Time ticks) = ticks
{-# INLINE unTime #-}

instance Show Time where
    show t = H.timePrint ISO8601_Precise (convertToElapsed t)

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
    reduceDateTime = convertFromElapsed . H.timeGetElapsedP

{- |
Convert between different representations of time. Our 'Time' timestamp has
nanosecond precision so converting from a type with  lesser or greater
precision will require you to either pad with zeros or to round to the nearest
nanosecond (who the hell has picoseconds of anything anyway?) if writing an
instance of this type.

The most important instance is probably the 'UTCTime' one, as many other
Haskell libraries use this type to represent time.

@since 0.3.3
-}
class Instant a where
    fromTime :: Time -> a
    intoTime :: a -> Time

{- |
Number of nanoseconds since the epoch.
-}
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
    in  Time . fromIntegral . nano

convertToPosix :: Time -> POSIXTime
convertToPosix = fromRational . (/ 1e9) . fromIntegral . unTime

{- |
Convert to the elapsed time with sub-second precision type from __hourglass__,
giving you ready access to that library's time formatting and calendar date
manipulation functions.
-}
instance Instant H.ElapsedP where
    fromTime = convertToElapsed
    intoTime = convertFromElapsed

convertFromElapsed :: H.ElapsedP -> Time
convertFromElapsed (H.ElapsedP (H.Elapsed (H.Seconds seconds)) (H.NanoSeconds nanoseconds)) =
    let s = fromIntegral seconds :: Int64
        ns = fromIntegral nanoseconds
    in  Time $! (s * 1000000000) + ns

convertToElapsed :: Time -> H.ElapsedP
convertToElapsed (Time ticks) =
    let (s, ns) = divMod ticks 1000000000
    in  H.ElapsedP (H.Elapsed (H.Seconds (s))) (H.NanoSeconds (ns))

{- |
This instance may be useful if you need to work with calendar dates with
functions from __time__. From here you would probably be interested in
'Data.Time.Calendar.toGregorian'. If you convert from a 'Day' it will be the
timestamp of midnight 00:00:00.0 on that date.

@since 0.3.5
-}
instance Instant Day where
    fromTime = utctDay . fromTime
    intoTime x = intoTime (UTCTime {utctDay = x, utctDayTime = 0})

instance Aeson.ToJSON Time where
    toEncoding = Aeson.string . H.timePrint ISO8601_Precise . convertToElapsed

instance Aeson.FromJSON Time where
    parseJSON (Aeson.String value) =
        let str = (fromRope (intoRope value))
            result = parseInput str
        in  case result of
                Just t -> pure t
                Nothing -> fail "Unable to parse input as a TimeStamp"
    parseJSON (invalid) = Aeson.typeMismatch "TimeStamp" invalid

{- |
Get the current system time, expressed as a 'Time' (which is to
say, number of nanoseconds since the Unix epoch).

@since 0.3.3
-}
getCurrentTimeNanoseconds :: IO Time
getCurrentTimeNanoseconds = do
    p <- H.timeCurrentP
    return $! convertFromElapsed p

{- |
The occasion of the Unix epoch, 1970-01-01T00:00:00.0Z.

@since 0.3.3
-}
epochTime :: Time
epochTime = Time 0
