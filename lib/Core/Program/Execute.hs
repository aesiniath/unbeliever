{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Embelish a Haskell command-line program with useful behaviours.

/Runtime/

Sets number of capabilities (heavy-weight operating system threads used by
the GHC runtime to run Haskell green threads) to the number of CPU cores
available (for some reason the default is 1 capability only, which is a bit
silly on a multicore system).

Install signal handlers to properly terminate the program performing
cleanup as necessary.

/Logging and output/

The 'Program' monad provides functions for both normal output and debug
logging. A common annoyance when building command line tools and daemons is
getting program output to @stdout@ and debug messages interleaved, made
even worse when error messages written to @stderr@ land in the same
console. To avoid this, when all output is sent through a single channel.
This includes both normal output and log messages.

/Exceptions/

Ideally your code should handle (and not leak) exceptions, as is good
practice anywhere in the Haskell ecosystem. As a measure of last resort
however, if an exception is thrown (and not caught) by your program it will
be caught at the outer 'execute' entrypoint, logged for debugging, and then
your program will exit.

/Customizing the execution context/

The 'execute' function will run your 'Program' in a basic 'Context'
initialized with appropriate defaults. While some settings can be changed
at runtime, if you need to replace (for example) the logging subsystem you
can initialize your program using 'configure' and then run 'executeWith'.
-}
module Core.Program.Execute
    (   Program ()
        {-* Running programs -}
      , configure
      , execute
      , executeWith
        {-* Exiting a program -}
      , terminate
        {-* Accessing program context -}
      , setProgramName
      , getProgramName
      , getCommandLine
        {-* Useful actions -}
      , write
      , writeS
      , fork
      , sleep
        {-* Internals -}
      , Context
    ) where

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
import Control.Monad.Catch (Handler(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import qualified Data.ByteString as B (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import qualified Data.ByteString.Lazy as L (hPut)
import Data.Hourglass (timePrint, TimeFormatElem(..))
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import Time.System (timezoneCurrent)

import Core.Text.Bytes
import Core.Text.Rope
import Core.System.External
import Core.Program.Context
import Core.Program.Logging
import Core.Program.Signal
import Core.Program.Arguments

unwrapProgram :: Program a -> ReaderT (MVar Context) IO a
unwrapProgram (Program reader) = reader

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

--
-- If an exception escapes, we'll catch it here. The displayException
-- value for some exceptions is really quit unhelpful, so we pattern
-- match the wrapping gumpf away for cases as we encounter them. The
-- final entry is the catch-all; the first is what we get from the
-- terminate action.
--
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
        text = intoRope (displayException e)
      in do
        runProgram context (event text)
        putMVar quit (ExitFailure 127)


{-|
Embelish a program with useful behaviours. See module header
"Core.Program.Execute" for a detailed description. Internally this function
calls 'configure' with an appropriate default when initializing.
-}
execute :: Program a -> IO ()
execute program = do
    context <- configure baselineConfig
    executeWith context program

{-|
Embelish a program with useful behaviours, supplying a configuration
for command-line options & argument parsing.
-}
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


processStandardOutput :: TChan Rope -> IO ()
processStandardOutput output = do
    forever $ do
        text <- atomically (readTChan output)

        B.hPut stdout (fromRope text) -- FIXME efficiency?
        B.hPut stdout (C.singleton '\n')

processDebugMessages :: TChan Message -> IO ()
processDebugMessages logger = do
    forever $ do
        Message now severity text potentialValue <- atomically (readTChan logger)

        return ()

{-|
Safely exit the program with the supplied exit code. Current output and
debug queues will be flushed, and then the process will terminate.
-}
terminate :: Int -> Program ()
terminate code =
  let
    exit = case code of
        0 -> ExitSuccess
        _ -> ExitFailure code
  in do
    v <- ask
    liftIO (Safe.throw exit)


{-|
Override the program name used for logging, etc. At least, that was the
idea. Nothing makes use of this at the moment. @:/@
-}
setProgramName :: Rope -> Program ()
setProgramName name = do
    v <- ask
    context <- liftIO (readMVar v)
    let context' = context {
        programNameFrom = name
    }
    liftIO (modifyMVar_ v (\_ -> pure context'))

{-|
Get the program name as invoked from the command-line (or as overridden by
'setProgramName').
-}
getProgramName :: Program Rope
getProgramName = do
    v <- ask
    context <- liftIO (readMVar v)
    return (programNameFrom context)

{-|
Write the supplied text to @stdout@.

This is for normal program output.

@
     'write' "Beginning now"
@
-}
write :: Rope -> Program ()
write text = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        let chan = outputChannelFrom context

        atomically (writeTChan chan text)

{-|
Call 'show' on the supplied argument and write the resultant text to
@stdout@.

(This is the equivalent of 'print' from __base__)
-}
writeS :: Show a => a -> Program ()
writeS = write . intoRope . show

{-|
Write the supplied bytes to the given handle
(in contrast to 'write' we don't output a trailing newline)
-}
output :: Handle -> Bytes -> Program ()
output h b = liftIO $ do
        B.hPut h (fromBytes b)

{-|
Fork a thread.
-}
--
-- TODO change Async to a wrapper called Thread
-- TODO documentation about launching threads
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

{-|
Pause the current thread for the given number of seconds. For
example, to delay a second and a half, do:

@
    'sleep' 1.5
@

(this wraps __base__'s 'threadDelay')
-}
--
-- FIXME is this the right type, given we want to avoid type default warnings?
--
sleep :: Rational -> Program () 
sleep seconds =
  let
    us = floor (toRational (seconds * 1e6))
  in
    liftIO $ threadDelay us

{-|
Retrieve the values of parameters parsed from options and arguments
supplied by the user on the command-line.
-}
getCommandLine :: Program (Parameters)
getCommandLine = do
    v <- ask
    liftIO $ do
        context <- readMVar v
        return (commandLineFrom context)
