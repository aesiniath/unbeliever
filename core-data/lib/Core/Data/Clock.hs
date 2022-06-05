{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Core.Data.Clock (
    -- * Time type
    TimeStamp,
    getCurrentTimeNanoseconds,

    -- * Conversions
    Instant (fromTime, intoTime),
    unTimeStamp,
) where

import Core.Text.Bytes (Bytes)
import Core.Text.Rope (Rope)
import Data.Bifoldable (Bifoldable)
import Data.ByteString qualified as B (ByteString)
import Data.Int (Int64)
import GHC.Generics

{- |
Number of nanoseconds since the Unix epoch.

The Show instance displays the TimeStamp as seconds with the nanosecond
precision expressed as a decimal amount after the interger, ie:

>>> t <- getCurrentTimeNanoseconds
>>> show t
2014-07-31T23:09:35.274387031Z

However this doesn't change the fact the underlying representation counts
nanoseconds since epoch:

>>> show $ unTimeStamp t
1406848175274387031

There is a Read instance that is reasonably accommodating.

>>> read "2014-07-31T13:05:04.942089001Z" :: TimeStamp
2014-07-31T13:05:04.942089001Z

>>> read "1406811904.942089001" :: TimeStamp
2014-07-31T13:05:04.942089001Z

>>> read "1406811904" :: TimeStamp
2014-07-31T13:05:04.000000000Z

In case you're wondering, the valid range of nanoseconds that fits into the
underlying Int64 is:

>>> show $ minBound :: TimeStamp
1677-09-21T00:12:43.145224192Z

>>> show $ maxBound :: TimeStamp
2262-04-11T23:47:16.854775807Z

so in a quarter millenium's time, yes, you'll have the Y2262 Problem.
Haskell code from today will, of course, still be running, so in the mid
Twenty-Third century you will need to replace this implementation with
something else.
-}
newtype TimeStamp = TimeStamp
    { unTimeStamp :: Int64
    }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bounded, Generic)

class Instant a where
    fromTime :: TimeStamp -> a
    intoTime :: a -> TimeStamp

instance Instant Int64 where
    fromTime = unTimeStamp
    intoTime = TimeStamp

instance Instant UTCTime where
    fromTime = undefined
    intoTime = undefined

getCurrentTimeNanoseconds :: IO TimeStamp
getCurrentTimeNanoseconds = undefined
