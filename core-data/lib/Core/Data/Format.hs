{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_HADDOCK hide #-}

module Core.Data.Format where

import Data.Hourglass qualified as H (
    TimeFormat (..),
    TimeFormatElem (..),
    TimeFormatString (..),
 )

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
