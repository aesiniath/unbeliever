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
initialized with appropriate defaults. Most settings can be changed at
runtime, but to specify the allowed command-line options and expected
arguments you can initialize your program using 'configure' and then run
with 'executeWith'.
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
      , getProgramName
      , setProgramName
      , getVerbosityLevel
      , setVerbosityLevel
      , getCommandLine
      , getApplicationState
      , setApplicationState
      , retrieve
      , update
        {-* Useful actions -}
      , write
      , writeS
      , writeR
      , output
        {-* Concurrency -}
      , Thread
      , fork
      , sleep
        {-* Internals -}
      , Context
      , None(..)
      , isNone
      , unProgram
      , unThread
    ) where

import Prelude hiding (log)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, link, cancel
    , ExceptionInLinkedThread(..), AsyncCancelled)
import Control.Concurrent.MVar (readMVar, putMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, check)
import Control.Concurrent.STM.TQueue (TQueue, readTQueue
    , writeTQueue, isEmptyTQueue)
import qualified Control.Exception as Base (throwIO)
import Control.Exception.Safe (SomeException, Exception(displayException))
import qualified Control.Exception.Safe as Safe (throw, catchesAsync)
import Control.Monad (when, forever)
import Control.Monad.Catch (Handler(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import qualified Data.ByteString as B (hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import System.Exit (ExitCode(..))

import Core.Text.Bytes
import Core.Text.Rope
import Core.Text.Utilities
import Core.System.Base
import Core.Program.Context
import Core.Program.Logging
import Core.Program.Signal
import Core.Program.Arguments

unProgram :: Program τ α -> ReaderT (Context τ) IO α
unProgram (Program reader) = reader

runProgram :: Context τ -> Program τ a -> IO a
runProgram context (Program reader) = do
    runReaderT reader context


-- execute actual "main"
executeAction :: Context τ -> Program τ α -> IO ()
executeAction context program =
  let
    quit = exitSemaphoreFrom context
  in do
    _ <- runProgram context program
    putMVar quit ExitSuccess

--
-- If an exception escapes, we'll catch it here. The displayException
-- value for some exceptions is really quit unhelpful, so we pattern
-- match the wrapping gumpf away for cases as we encounter them. The
-- final entry is the catch-all; the first is what we get from the
-- terminate action.
--
escapeHandlers :: Context c -> [Handler IO ()]
escapeHandlers context = [
    Handler (\ (exit :: ExitCode) -> done exit)
  , Handler (\ (_ :: AsyncCancelled) -> pass)
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
execute :: Program None α -> IO ()
execute program = do
    context <- configure None (simple [])
    executeWith context program

{-|
Embelish a program with useful behaviours, supplying a configuration
for command-line options & argument parsing and an initial value for
the top-level application state, if appropriate.
-}
executeWith :: Context τ -> Program τ α -> IO ()
executeWith context program = do
    -- command line +RTS -Nn -RTS value
    when (numCapabilities == 1) (getNumProcessors >>= setNumCapabilities)

    let quit = exitSemaphoreFrom context
        level = verbosityLevelFrom context
        out = outputChannelFrom context
        log = loggerChannelFrom context

    -- set up standard output
    o <- async $ do
        processStandardOutput out

    -- set up debug logger
    l <- async $ do
        processDebugMessages log

    -- set up signal handlers
    _ <- async $ do
        setupSignalHandlers quit level

    -- run actual program, ensuring to trap uncaught exceptions
    m <- async $ do
        Safe.catchesAsync
            (executeAction context program)
            (escapeHandlers context)

    code <- readMVar quit
    cancel m

    -- drain message queues
    atomically $ do
        done2 <- isEmptyTQueue log
        check done2

        done1 <- isEmptyTQueue out
        check done1

    threadDelay 100 -- instead of yield
    hFlush stdout

    cancel l
    cancel o

    -- exiting this way avoids "Exception: ExitSuccess" noise in GHCi
    if code == ExitSuccess
        then return ()
        else (Base.throwIO code)


processStandardOutput :: TQueue Rope -> IO ()
processStandardOutput out = do
    forever $ do
        text <- atomically (readTQueue out)

        hWrite stdout text
        B.hPut stdout (C.singleton '\n')

processDebugMessages :: TQueue Message -> IO ()
processDebugMessages log = do
    forever $ do
        -- TODO do sactually do something with log messages
        -- Message now severity text potentialValue <- ...
        _ <- atomically (readTQueue log)

        return ()

{-|
Safely exit the program with the supplied exit code. Current output and
debug queues will be flushed, and then the process will terminate.
-}
terminate :: Int -> Program τ ()
terminate code =
  let
    exit = case code of
        0 -> ExitSuccess
        _ -> ExitFailure code
  in do
    liftIO (Safe.throw exit)

-- undocumented
getVerbosityLevel :: Program τ Verbosity
getVerbosityLevel = do
    context <- ask
    liftIO $ do
        level <- readMVar (verbosityLevelFrom context)
        return level

{-|
Change the verbosity level of the program's logging output. This changes
whether 'event' and the 'debug' family of functions emit to the logging
stream; they do /not/ affect 'write'ing to the terminal on the standard
output stream.
-}
setVerbosityLevel :: Verbosity -> Program τ ()
setVerbosityLevel level = do
    context <- ask
    liftIO $ do
        let v = verbosityLevelFrom context
        modifyMVar_ v (\_ -> pure level)


{-|
Override the program name used for logging, etc. At least, that was the
idea. Nothing makes use of this at the moment. @:/@
-}
setProgramName :: Rope -> Program τ ()
setProgramName name = do
    context <- ask
    liftIO $ do
        let v = programNameFrom context
        modifyMVar_ v (\_ -> pure name)

{-|
Get the program name as invoked from the command-line (or as overridden by
'setProgramName').
-}
getProgramName :: Program τ Rope
getProgramName = do
    context <- ask
    liftIO $ do
        let v = programNameFrom context
        readMVar v

{-|
Get the user supplied application state as originally supplied to
'configure' and modified subsequntly by replacement with
'setApplicationState'.

@
    state <- getApplicationState
@
-}
getApplicationState :: Program τ τ
getApplicationState = do
    context <- ask
    liftIO $ do
        let v = applicationDataFrom context
        readMVar v

{-|
Update the user supplied top-level application state.

@
    let state' = state { answer = 42 }
    setApplicationState state'
@
-}
setApplicationState :: τ -> Program τ ()
setApplicationState user = do
    context <- ask
    liftIO $ do
        let v = applicationDataFrom context
        modifyMVar_ v (\_ -> pure user)

{-|
Alias for 'getApplicationState'.
-}
retrieve :: Program τ τ
retrieve = getApplicationState

{-|
Alias for 'setApplicationState'.
-}
update :: τ -> Program τ ()
update = setApplicationState

{-|
Write the supplied text to @stdout@.

This is for normal program output.

@
     'write' "Beginning now"
@
-}
write :: Rope -> Program τ ()
write text = do
    context <- ask
    liftIO $ do
        let out = outputChannelFrom context

        atomically (writeTQueue out text)

{-|
Call 'show' on the supplied argument and write the resultant text to
@stdout@.

(This is the equivalent of 'print' from __base__)
-}
writeS :: Show α => α -> Program τ ()
writeS = write . intoRope . show

{-|
Pretty print the supplied argument and write the resultant text to
@stdout@. This will pass the detected terminal width to the 'render'
function, resulting in appopriate line wrapping when rendering your value.
-}
writeR :: Render α => α -> Program τ ()
writeR thing = do
    context <- ask
    liftIO $ do
        let out = outputChannelFrom context
        let columns = terminalWidthFrom context

        let text = render columns thing

        atomically (writeTQueue out text)

{-|
Write the supplied @Bytes@ to the given @Handle@. Note that in contrast to
'write' we don't output a trailing newline.

@
    output h b
@

Do not use this to output to @stdout@ as that would bypass the mechanism
used by the @write*@ functions to sequence output correctly. If you wish to
write to the terminal use:

@
    write (intoRope b)
@

(which is not /unsafe/, but will lead to unexpected results if the binary
blob you pass in is not UTF-8 text).
-}
output :: Handle -> Bytes -> Program τ ()
output h b = liftIO $ do
        B.hPut h (fromBytes b)

{-|
A thread for concurrent computation. Haskell uses green threads: small
lines of work that are scheduled down onto actual execution contexts, set
by default by this library to be one per core. They are incredibly
lightweight, and you are encouraged to use them freely. Haskell provides a
rich ecosystem of tools to do work concurrently and to communicate safely
between threads

(this wraps __async__'s 'Async')
-}
newtype Thread α = Thread (Async α)

unThread :: Thread α -> Async α
unThread (Thread a) = a

{-|
Fork a thread. The child thread will run in the same @Context@ as the
calling @Program@, including sharing the user-defined application state
type.

(this wraps __async__'s 'async' which in turn wraps __base__'s 'Control.Concurrent.forkIO')
-}
fork :: Program τ α -> Program τ (Thread α)
fork program = do
    context <- ask
    liftIO $ do
        a <- async $ do
            runProgram context program
        link a
        return (Thread a)

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
sleep :: Rational -> Program τ ()
sleep seconds =
  let
    us = floor (toRational (seconds * 1e6))
  in
    liftIO $ threadDelay us

{-|
Retrieve the values of parameters parsed from options and arguments
supplied by the user on the command-line.
-}
getCommandLine :: Program τ (Parameters)
getCommandLine = do
    context <- ask
    return (commandLineFrom context)
