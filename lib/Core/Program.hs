{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Core.Program
    ( execute
    , Program
    , terminate
    , setProgramName
    , getProgramName
    , write
    , writeS
    , debug
    , fork
    , sleep
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, link, cancel,
    ExceptionInLinkedThread(..))
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
    putMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, check)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan,
    writeTChan, isEmptyTChan)
import Control.Exception.Safe (SomeException, catchAsync,
    Exception(displayException))
import qualified Control.Exception.Safe as Safe (throw)
import Control.Monad (when, forever)
import qualified Control.Monad.Catch as Original (MonadThrow(throwM),
    catches, Handler(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString.Lazy as L (hPut)
import Data.Fixed
import Data.Hourglass (timePrint, TimeFormatElem(..))
import qualified Data.Text.IO as T
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import Time.System (timezoneCurrent)

import Core.Text
import Core.System
import Core.Render

data Context = Context {
      contextProgramName :: Text
    , contextExitSemaphore :: MVar ExitCode
    , contextStartTime :: TimeStamp
    , contextOutput :: TChan Text
    , contextLogger :: TChan Message
}

data Message = Message TimeStamp Nature Text

data Nature = Output | Debug

{-
    FIXME
    Change to global quit semaphore, reachable anywhere?
-}

instance Semigroup Context where
    (<>) one two = Context {
          contextProgramName = (contextProgramName two)
        , contextExitSemaphore = (contextExitSemaphore one)
        , contextStartTime = contextStartTime one
        , contextOutput = contextOutput one
        , contextLogger = contextLogger one
        }

--
-- The type of a top-level Prgoram.
--
-- You would use this by writing:
--
-- > module Main where
-- >
-- > import Core.Program
-- >
-- > main :: IO ()
-- > main = execute program
--
-- and defining a program that is the top level of your application:
--
-- > program :: Program ()
--
-- Program actions are combinable; you can sequence them (using bind in
-- do-notation) or run them in parallel, but basically you should need
-- one such object at the top of your application.
--
-- You're best off putting your top-level Program action in a separate
-- module so you can refer to it from test suites and example snippets.
--
newtype Program a = Program (ReaderT (MVar Context) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MVar Context))

unwrapProgram :: Program a -> ReaderT (MVar Context) IO a
unwrapProgram (Program reader) = reader

instance Render TimeStamp where
    render t = intoText (show t)

--
-- This is complicated. The **safe-exceptions** library exports a
-- `throwM` which is not the `throwM` class method from MonadThrow.
-- See https://github.com/fpco/safe-exceptions/issues/31 for
-- discussion. Not sure if this is right, but Program needs a
-- MonadThrow instance.
--
instance Original.MonadThrow Program where
    throwM = liftIO . Safe.throw

runProgram :: Context -> Program a -> IO a
runProgram context (Program reader) = do
    v <- newMVar context
    runReaderT reader v


-- execute actual "main"
executeAction :: Context -> Program a -> IO ()
executeAction context program =
  let
    quit = contextExitSemaphore context
  in do
    runProgram context program
    putMVar quit ExitSuccess



-- if an exception escapes, we'll catch it here
escapeHandlers :: Context -> [Original.Handler IO ()]
escapeHandlers context = [
    Original.Handler (\ (ExceptionInLinkedThread _ e) -> bail context e)
    ]

bail :: Exception e => Context -> e -> IO ()
bail context e =
  let
    quit = contextExitSemaphore context
  in do
    runProgram context (debug (intoText (displayException e)))
    putMVar quit (ExitFailure 127)


--
-- | Embelish a program with useful behaviours.
--
-- Sets number of capabilities (heavy weight operating system threads used to
-- run Haskell green threads) to the number of CPU cores available.
--
execute :: Program a -> IO ()
execute program = do
    -- command line +RTS -Nn -RTS value
    when (numCapabilities == 1) (getNumProcessors >>= setNumCapabilities)

    name <- getProgName
    quit <- newEmptyMVar
    start <- getCurrentTimeNanoseconds
    output <- newTChanIO
    logger <- newTChanIO

    let context = Context (intoText name) quit start output logger

    -- set up standard output
    async $ do
        processStandardOutput output

    -- set up debug logger
    async $ do
        processDebugMessages logger

    -- run actual program, ensuring to trap uncaught exceptions
    async $ do
        Original.catches
            (executeAction context program)
            (escapeHandlers context)

    code <- readMVar quit

    -- drain message queues
    atomically $ do
        done2 <- isEmptyTChan logger
        check done2

        done1 <- isEmptyTChan output
        check done1

    hFlush stdout
    exitWith code

--
-- | Safely exit the program with the supplied exit code. Current
-- output and debug queues will be flushed, and then the process will
-- terminate.
--
terminate :: Int -> Program ()
terminate code =
  let
    exit = case code of
        0 -> ExitSuccess
        _ -> ExitFailure code
  in do
    v <- ask
    liftIO $ do
        context <- readMVar v
        let quit = contextExitSemaphore context
        putMVar quit exit

--
--
-- | Override the program name used for logging, etc
--
setProgramName :: Text -> Program ()
setProgramName name = do
    v <- ask
    context <- liftIO (readMVar v)
    let context' = context {
        contextProgramName = name
    }
    liftIO (modifyMVar_ v (\_ -> pure context'))

getProgramName :: Program Text
getProgramName = do
    v <- ask
    context <- liftIO (readMVar v)
    return (contextProgramName context)

--
-- | Write the supplied text to @stdout@.
--
-- This is for normal program output.
--
-- >     write "Beginning now"
--
write :: Text -> Program ()
write text = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        let chan = contextOutput context

        atomically (writeTChan chan text)

--
-- | Call 'show' on the supplied argument and write the resultant
-- text to @stdout@.
--
-- (This is the equivalent of 'print' from base)
--
writeS :: Show a => a -> Program ()
writeS = write . intoText . show

--
-- | Write the supplied bytes to the given handle
-- (in contrast to 'write' we don't output a trailing newline)
--
output :: Handle -> Bytes -> Program ()
output h b = liftIO $ do
        S.hPut h (fromBytes b)

--
-- | Output a debugging message. This:
--
-- >    debug "Starting..."
--
-- Will result in
--
-- > 13:05:55Z (0000.000019) Starting...
--
-- appearing on stdout /and/ the message being sent down the logging
-- channel. The output string is current time in UTC, and time elapsed
-- since startup shown to the nearest millisecond (timestamps are to
-- nanosecond precision, but you don't need that kind of resolution in
-- in ordinary debugging)
--
debug :: Text -> Program ()
debug text = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        let start = contextStartTime context
        let output = contextOutput context
        let logger = contextLogger context

        now <- getCurrentTimeNanoseconds

        let result = formatDebugMessage start now text

        atomically $ do
            writeTChan output result
            writeTChan logger (Message now Debug result)

debugS :: Show a => a -> Program ()
debugS = debug . intoText . show

formatDebugMessage :: TimeStamp -> TimeStamp -> Text -> Text
formatDebugMessage start now message =
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
        [ intoText stampZ
        , " ("
        , padWithZeros 9 (show elapsed)
        , ") "
        , render message
        ]

--
-- | Utility function to prepend \'0\' characters to a string representing a
-- number.
--
{-
    Cloned from **locators** package Data.Locators.Hashes, BSD3 licence
-}
padWithZeros :: Int -> String -> Text
padWithZeros digits str =
    intoText (pad ++ str)
  where
    pad = take len (replicate digits '0')
    len = digits - length str


processDebugMessages :: TChan Message -> IO ()
processDebugMessages logger = do
    forever $ do
        message <- atomically (readTChan logger)
 
        -- TODO do something with message

        return ()


processStandardOutput :: TChan Text -> IO ()
processStandardOutput output = do
    forever $ do
        text <- atomically (readTChan output)

        S.hPut stdout (fromText text)
        S.hPut stdout (C.singleton '\n')

--
-- Fork a thread
-- TODO change Async to a wrapper called Thread
-- TODO documentation HERE
--
fork :: Program a -> Program (Async a)
fork program = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        a <- async $ do
            runProgram context program
        link a
        return a

--
-- | Pause the current thread for the given number of seconds. For
-- example, to delay a second and a half, do:
--
-- >     sleep 1.5
--
-- (this wraps base's 'threadDelay')
--
{-
    FIXME is this the right type, given we want to avoid type default warnings?
-}
sleep :: Rational -> Program () 
sleep seconds =
  let
    us = floor (toRational (seconds * 1e6))
  in
    liftIO $ threadDelay us

