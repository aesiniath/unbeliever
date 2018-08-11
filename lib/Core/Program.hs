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
import Data.Hourglass (timePrint, localTimePrint, localTimeSetTimezone, localTimeFromGlobal, TimeFormatElem(..))
import Time.System (timezoneCurrent)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (when, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Concurrent.Async (async, link)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
    putMVar, modifyMVar_)
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString.Lazy as L (hPut)
import Data.Fixed
import qualified Data.Text.IO as T
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import qualified Streamly.Prelude as Streamly
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Unsafe (unsafePerformIO)

import Core.Text
import Core.System
import Core.Logging
import Core.Render

data Context = Context {
      contextProgramName :: Text
    , contextExitSemaphore :: MVar ExitCode
    , contextStartTime :: TimeStamp
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
        write stdout line

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
    chan <- newTChanIO

    let context = Context (intoText name) quit start chan

    -- set up debug logger
    async $ do
        outputDebugMessages start chan

    -- set up terminator
    async $ do
        code <- readMVar quit
        exitWith code

    executeAction context program
    return ()

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
-- | Write the supplied text to the given handle.
--
-- Common use is debugging:
--
-- >     write stdout "Beginning now"
--
write :: Handle -> Text -> Program ()
write h t = liftIO $ do
--  T.hPutStrLn h (fromText t)
    S.hPut h (fromText t)
    S.hPut h (C.singleton '\n')

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


outputDebugMessages :: TimeStamp -> TChan Text -> IO ()
outputDebugMessages start chan = do
    forever $ do
        message <- atomically (readTChan chan)

        now <- getCurrentTimeNanoseconds

        let result = formatDebugMessage start now message

        S.hPut stdout (fromText result)
        S.hPut stdout (C.singleton '\n')

