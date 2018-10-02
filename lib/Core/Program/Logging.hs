{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

module Core.Program.Logging
    (
        putMessage
      , event
      , debug
      , debugS
      , debugR
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.Reader.Class (MonadReader(ask))
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Fixed
import Data.Hourglass (timePrint, TimeFormatElem(..))
import qualified Data.Text.Short as S (replicate)
import Time.System (timezoneCurrent)

import Core.Text.Bytes
import Core.Text.Rope
import Core.Text.Utilities
import Core.System.Base
import Core.Program.Context

{-
class Monad m => MonadLog a m where
    logMessage :: Monoid a => Severity -> a -> m () 

instance MonadLog Text IO where
    logMessage severity message = do
        tick <- getCurrentTimeNanoseconds
        
        let line = show tick ++ " [" ++ show severity ++ "] " ++ show message
        hPutStrLn stdout line
-}

putMessage :: Context τ -> Message -> IO ()
putMessage context message@(Message now nature text potentialValue) = do
    let start = startTimeFrom context
    let width = terminalWidthFrom context
    let output = outputChannelFrom context
    let logger = loggerChannelFrom context

    let display = case potentialValue of
            Just value ->
                if contains '\n' value
                    then text <> " =\n" <> value
                    else text <> " = " <> value
            Nothing -> text

    let result = formatLogMessage start now display

    atomically $ do
        writeTChan output result
        writeTChan logger message


formatLogMessage :: TimeStamp -> TimeStamp -> Rope -> Rope
formatLogMessage start now message =
  let
    start' = unTimeStamp start
    now' = unTimeStamp now
    stampZ = timePrint
        [ Format_Hour
        , Format_Text ':'
        , Format_Minute
        , Format_Text ':'
        , Format_Second
        , Format_Text 'Z'
        ] now

    -- I hate doing math in Haskell
    elapsed = fromRational (toRational (now' - start') / 1e9) :: Fixed E3
  in
    mconcat
        [ intoRope stampZ
        , " ("
        , padWithZeros 9 (show elapsed)
        , ") "
        , message
        ]

--
-- | Utility function to prepend \'0\' characters to a string representing a
-- number.
--
{-
    Cloned from **locators** package Data.Locators.Hashes, BSD3 licence
-}
padWithZeros :: Int -> String -> Rope
padWithZeros digits str =
    intoRope pad <> intoRope str
  where
    pad = S.replicate len "0"
    len = digits - length str

{-|
Note a significant event, state transition, status, or debugging
message. This:

@
    'event' "Starting..."
@

will result in

> 13:05:55Z (0000.001) Starting...

appearing on stdout /and/ the message being sent down the logging
channel. The output string is current time in UTC, and time elapsed
since startup shown to the nearest millisecond (our timestamps are to
nanosecond precision, but you don't need that kind of resolution in
in ordinary debugging).

Messages sent to syslog will be logged at @Info@ level severity.
-}
event :: Rope -> Program τ ()
event text = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        now <- getCurrentTimeNanoseconds
        putMessage context (Message now Event text Nothing)

{-|
Output a debugging message formed from a label and a value. This is like
'event' above but for the (rather common) case of needing to inspect or
record the value of a variable when debugging code.  This:

@
    'setProgramName' \"hello\"
    name <- 'getProgramName'
    'debug' \"programName\" name
@

will result in

> 13:05:58Z (0003.141) programName = hello

appearing on stdout /and/ the message being sent down the logging channel,
assuming these actions executed about three seconds after program start.

Messages sent to syslog will be logged at @Debug@ level severity.
-}
debug :: Rope -> Rope -> Program τ ()
debug label value = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        now <- getCurrentTimeNanoseconds
        putMessage context (Message now Debug label (Just value))

{-|
Convenience for the common case of needing to inspect the value
of a general variable which has a Show instance
-}
debugS :: Show α => Rope -> α -> Program τ ()
debugS label value = debug label (intoRope (show value))

{-|
Convenience for the common case of needing to inspect the value of a
general variable for which there is a 'Render' instance and so can pretty
print the supplied argument to the log. This will pass the detected
terminal width to the 'render' function, resulting in appopriate line
wrapping when rendering your value (if logging to something other than
console the default width of @80@ will be applied).
-}
debugR :: Render α => Rope -> α -> Program τ ()
debugR label thing = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        now <- getCurrentTimeNanoseconds

        let width = terminalWidthFrom context

        -- TODO take into account width already consumed by timestamp
        -- TODO move render to putMessage? putMessageR?
        let value = render width thing

        putMessage context (Message now Debug label (Just value))
