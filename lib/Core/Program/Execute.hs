{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Program.Execute
    ( execute
    , Program
    , terminate
    , setProgramName
    , getProgramName
    , getCommandLine
    , write
    , writeS
    , event
    , debug
    , debugS
    , fork
    , sleep
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent (yield, threadDelay)
import Control.Concurrent.Async (Async, async, link, cancel, wait,
    ExceptionInLinkedThread(..), AsyncCancelled)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
    putMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, check)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan,
    writeTChan, isEmptyTChan)
import qualified Control.Exception as Base (throwIO)
import Control.Exception.Safe (SomeException, Exception(displayException))
import qualified Control.Exception.Safe as Safe (throw, catchesAsync)
import Control.Monad (when, forever)
import Control.Monad.Catch (MonadThrow(throwM), Handler(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString.Lazy as L (hPut)
import Data.Hourglass (timePrint, TimeFormatElem(..))
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import Time.System (timezoneCurrent)

import Core.Text
import Core.System
import Core.Render
import Core.Program.Context
import Core.Program.Logging
import Core.Program.Signal
import Core.Program.Arguments

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

{-
    This is complicated. The **safe-exceptions** library exports a
    `throwM` which is not the `throwM` class method from MonadThrow.
    See https://github.com/fpco/safe-exceptions/issues/31 for
    discussion. In any event, the re-exports flow back to
    Control.Monad.Catch from **exceptions** and Control.Exceptions in
    **base**. In _this_ module, we need to catch everything (including
    asynchronous exceptions); elsewhere we will use and wrap/export
    **safe-exceptions**'s variants of the functions.
-}
instance MonadThrow Program where
    throwM = liftIO . Safe.throw

runProgram :: Context -> Program a -> IO a
runProgram context (Program reader) = do
    v <- newMVar context
    runReaderT reader v


-- execute actual "main"
executeAction :: Context -> Program a -> IO ()
executeAction context program =
  let
    quit = exitSemaphoreFrom context
  in do
    runProgram context program
    putMVar quit ExitSuccess

{-
    If an exception escapes, we'll catch it here. The displayException
    value for some exceptions is really quit unhelpful, so we pattern
    match the wrapping gumpf away for cases as we encounter them. The
    final entry is the catch-all; the first is what we get from the
    terminate action.
-}
escapeHandlers :: Context -> [Handler IO ()]
escapeHandlers context = [
    Handler (\ (exit :: ExitCode) -> done exit)
  , Handler (\ (e :: AsyncCancelled) -> pass)
  , Handler (\ (ExceptionInLinkedThread _ e) -> bail e)
  , Handler (\ (e :: SomeException) -> bail e)
  ]
  where
    quit = exitSemaphoreFrom context

    pass :: IO ()
    pass = return ()

    done :: ExitCode -> IO ()
    done exit = do
        putMVar quit exit

    bail :: Exception e => e -> IO ()
    bail e =
      let
        text = intoText (displayException e)
      in do
        runProgram context (event text)
        putMVar quit (ExitFailure 127)


--
-- | Embelish a program with useful behaviours.
--
-- /Runtime/
--
-- Sets number of capabilities (heavy-weight operating system threads
-- used by the GHC runtime to run Haskell green threads) to the number
-- of CPU cores available (for some reason the default is 1 capability
-- only, which is a bit silly on a multicore system).
--
-- Install signal handlers to properly terminate the program
-- performing cleanup as necessary.
--
-- /Logging and output/
--
-- The Program monad provides functions for both normal output and
-- debug logging. A common annoyance when building command line
-- tools and daemons is getting program output to stdout and debug
-- messages interleaved, made even worse when error messages written to
-- stderr land in the same console. To avoid this, when using the
-- Program monad all output is sent through a single channel. This
-- includes both normal output and log messages.
--
-- /Exceptions/
--
-- Ideally your code should handle (and not leak) exceptions, as is
-- good practice anywhere in the Haskell ecosystem. As a measure of
-- last resort however, if an exception is thrown (and not caught) by
-- your program it will be caught here, logged for debugging, and then
-- your Program will exit.
--
-- /Customizing the execution context/
--
-- This function will run your Program in a basic 'Context'
-- initialized with appropriate defaults. While some settings can be
-- changed at runtime, if you need to replace (for example) the
-- logging subsystem you can run your program using 'configure' and
-- then 'executeWith'.
--
execute :: Program a -> IO ()
execute program = do
    let config = minimalConfig

    name <- getProgName
    parameters <- handleCommandLine config
    quit <- newEmptyMVar
    start <- getCurrentTimeNanoseconds
    width <- getConsoleWidth
    output <- newTChanIO
    logger <- newTChanIO

    let context = Context {
          programNameFrom = (intoText name)
        , commandLineFrom = parameters
        , exitSemaphoreFrom = quit
        , startTimeFrom = start
        , terminalWidthFrom = width
        , outputChannelFrom = output
        , loggerChannelFrom = logger
    }

    executeWith context program

executeWith :: Context -> Program a -> IO ()
executeWith context program = do
    -- command line +RTS -Nn -RTS value
    when (numCapabilities == 1) (getNumProcessors >>= setNumCapabilities)

    let quit = exitSemaphoreFrom context
        output = outputChannelFrom context
        logger = loggerChannelFrom context

    -- set up standard output
    o <- async $ do
        processStandardOutput output

    -- set up debug logger
    l <- async $ do
        processDebugMessages logger

    -- set up signal handlers
    s <- async $ do
        setupSignalHandlers quit

    -- run actual program, ensuring to trap uncaught exceptions
    m <- async $ do
        Safe.catchesAsync
            (executeAction context program)
            (escapeHandlers context)

    code <- readMVar quit
    cancel m

    -- drain message queues
    atomically $ do
        done2 <- isEmptyTChan logger
        check done2

        done1 <- isEmptyTChan output
        check done1

    threadDelay 100 -- instead of yield
    hFlush stdout

    cancel l
    cancel o

    -- exiting this way avoids "Exception: ExitSuccess" noise in GHCi
    if code == ExitSuccess
        then return ()
        else (Base.throwIO code)



processStandardOutput :: TChan Text -> IO ()
processStandardOutput output = do
    forever $ do
        text <- atomically (readTChan output)

        S.hPut stdout (fromText text)
        S.hPut stdout (C.singleton '\n')


processDebugMessages :: TChan Message -> IO ()
processDebugMessages logger = do
    forever $ do
        Message now severity text potentialValue <- atomically (readTChan logger)

        return ()

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
    liftIO (Safe.throw exit)


--
--
-- | Override the program name used for logging, etc
--
setProgramName :: Text -> Program ()
setProgramName name = do
    v <- ask
    context <- liftIO (readMVar v)
    let context' = context {
        programNameFrom = name
    }
    liftIO (modifyMVar_ v (\_ -> pure context'))

getProgramName :: Program Text
getProgramName = do
    v <- ask
    context <- liftIO (readMVar v)
    return (programNameFrom context)

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
        let chan = outputChannelFrom context

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
-- | Note a significant event, state transition, status, or debugging
-- message. This:
--
-- >    event "Starting..."
--
-- will result in
--
-- > 13:05:55Z (0000.001) Starting...
--
-- appearing on stdout /and/ the message being sent down the logging
-- channel. The output string is current time in UTC, and time elapsed
-- since startup shown to the nearest millisecond (our timestamps are to
-- nanosecond precision, but you don't need that kind of resolution in
-- in ordinary debugging).
-- 
-- Messages sent to syslog will be logged at @Info@ level severity.
--
event :: Text -> Program ()
event text = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        now <- getCurrentTimeNanoseconds
        putMessage context (Message now Event text Nothing)

--
-- | Output a debugging message formed from a label and a value. This
-- is like 'event' above but for the (rather common) case of needing
-- to inspect or record the value of a variable when debugging code.
-- This:
--
-- >    setProgramName "hello"
-- >    name <- getProgramName
-- >    debug "programName" name
--
-- will result in
--
-- > 13:05:58Z (0003.141) programName = hello
--
-- appearing on stdout /and/ the message being sent down the logging
-- channel, assuming these actions executed about three seconds after
-- program start.
--
-- Messages sent to syslog will be logged at @Debug@ level severity.
--
debug :: Text -> Text -> Program ()
debug label value = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        now <- getCurrentTimeNanoseconds
        putMessage context (Message now Debug label (Just value))


debugS :: Show a => Text -> a -> Program ()
debugS label value = debug label (intoText (show value))

--
-- Fork a thread
--
{-
    TODO change Async to a wrapper called Thread
    TODO documentation HERE
-}
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


handleCommandLine :: Config -> IO Parameters
handleCommandLine config = do
    argv <- getArgs
    let parameters = parseCommandLine config argv
    -- TODO DO SOMETHING
    return parameters


getCommandLine :: Program (Parameters)
getCommandLine = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        return (commandLineFrom context)
