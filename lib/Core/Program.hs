{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Core.Program
    ( execute
    , Program
    , setProgramName
    , getProgramName
    , write
    , write'
    , debug2
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent.Async (async, link, cancel)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
    putMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, check)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan,
    writeTChan, isEmptyTChan)
import Control.Monad (when, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString.Lazy as L (hPut)
import Data.Fixed
import Data.Hourglass (timePrint, localTimePrint, localTimeSetTimezone, localTimeFromGlobal, TimeFormatElem(..))
import qualified Data.Text.IO as T
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import Time.System (timezoneCurrent)

import Core.Text
import Core.System
import Core.Logging
import Core.Render

data Context = Context {
      contextProgramName :: Text
    , contextExitSemaphore :: MVar ExitCode
    , contextStartTime :: TimeStamp
    , contextOutput :: TChan Text
    , contextLogger :: TChan Text
}

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

instance MonadLog Text Program where
    logMessage severity message =
      let
        formatTime t = intoText (timePrint
            [ Format_Hour
            , Format_Text ':'
            , Format_Minute
            , Format_Text ':'
            , Format_Second
            , Format_Text '.'
            , Format_Precision 1
            , Format_Text 'Z'
            ] t)
      in do
        t <- liftIO getCurrentTimeNanoseconds

        let line = formatTime t <> " " <> render severity <> " " <> render message
        write line

instance Render TimeStamp where
    render t = intoText (show t)

executeAction :: Context -> Program a -> IO a
executeAction context (Program reader) = do
    v <- newMVar context
    runReaderT reader v

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
        processDebugMessages start output logger

    -- run program
    async $ do
        executeAction context program
        putMVar quit ExitSuccess

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
-- | Write the supplied text to stdout
--
-- Common use is debugging:
--
-- >     write "Beginning now"
--
write :: Text -> Program ()
write t = do
    v <- ask
    context <- liftIO (readMVar v)
    let output = contextOutput context

    liftIO (atomically (writeTChan output t))

--
-- | Write the supplied bytes to the given handle
-- (in contrast to 'write' we don't output a trailing newline)
--
write' :: Handle -> Bytes -> Program ()
write' h b = liftIO $ do
        S.hPut h (fromBytes b)

debug2 :: Text -> Program ()
debug2 message = do
    v <- ask
    context <- liftIO (readMVar v)
    let chan = contextLogger context

    liftIO (atomically (writeTChan chan message))


{-
    zone <- liftIO timezoneCurrent
    let stamp = localTimePrint
            [ Format_Hour
            , Format_Text ':'
            , Format_Minute
            , Format_Text ':'
            , Format_Second
            ] $ localTimeSetTimezone zone (localTimeFromGlobal t)
-}

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
--      , Format_Text '.'
--      , Format_Precision 1
        , Format_Text 'Z'
        ] now

    -- I hate doing math in Haskell
    elapsed = fromRational (toRational (now' - start') / 1e9) :: Fixed E6
  in
    mconcat
        [ intoText stampZ
        , " ("
        , padWithZeros 11 (show elapsed)
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


processDebugMessages :: TimeStamp -> TChan Text -> TChan Text -> IO ()
processDebugMessages start output logger = do
    forever $ do
        message <- atomically (readTChan logger)

        now <- getCurrentTimeNanoseconds

        let result = formatDebugMessage start now message

        atomically (writeTChan output result)

processStandardOutput :: TChan Text -> IO ()
processStandardOutput output = do
    forever $ do
        message <- atomically (readTChan output)

        S.hPut stdout (fromText message)
        S.hPut stdout (C.singleton '\n')

