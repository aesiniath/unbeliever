{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
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

Encoding is set to UTF-8, working around confusing bugs that sometimes
occur when applications are running in Docker containers.

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
      , getCommandLine
      , lookupOptionFlag
      , lookupOptionValue
      , lookupArgument
      , getProgramName
      , setProgramName
      , getVerbosityLevel
      , setVerbosityLevel
      , getApplicationState
      , setApplicationState
      , retrieve
      , update
        {-* Useful actions -}
      , output
      , input
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
      , invalid
    ) where

import Prelude hiding (log)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, link, cancel
    , ExceptionInLinkedThread(..), AsyncCancelled, race_)
import Control.Concurrent.MVar (readMVar, putMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, check)
import Control.Concurrent.STM.TQueue (TQueue, readTQueue, isEmptyTQueue)
import qualified Control.Exception as Base (throwIO)
import Control.Exception.Safe (SomeException, Exception(displayException))
import qualified Control.Exception.Safe as Safe (throw, catchesAsync)
import Control.Monad (when, forever)
import Control.Monad.Catch (Handler(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader(ask))
import qualified Data.ByteString as B (hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import GHC.Conc (numCapabilities, getNumProcessors, setNumCapabilities)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Exit (ExitCode(..))
import qualified System.Posix.Process as Posix (exitImmediately)

import Core.Data.Structures
import Core.Text.Bytes
import Core.Text.Rope
import Core.System.Base
import Core.Program.Context
import Core.Program.Logging
import Core.Program.Signal
import Core.Program.Arguments

-- execute actual "main"
executeAction :: Context τ -> Program τ α -> IO ()
executeAction context program =
  let
    quit = exitSemaphoreFrom context
  in do
    _ <- subProgram context program
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
        subProgram context $ do
            setVerbosityLevel Debug
            event text
        putMVar quit (ExitFailure 127)

--
-- If an exception occurs in one of the output handlers, its failure causes
-- a subsequent race condition when the program tries to clean up and drain
-- the queues. So we use `exitImmediately` (which we normally avoid, as it
-- unhelpfully destroys the parent process if you're in ghci) because we
-- really need the process to go down and we're in an inconsistent state
-- where debug or console output is no longer possible.
--
collapseHandlers :: [Handler IO ()]
collapseHandlers =
  [ Handler (\ (e :: AsyncCancelled) -> do
                Base.throwIO e)
  , Handler (\ (e :: SomeException) -> do
                putStrLn "error: Output handler collapsed"
                print e
                Posix.exitImmediately (ExitFailure 99))
  ]

{-|
Embelish a program with useful behaviours. See module header
"Core.Program.Execute" for a detailed description. Internally this function
calls 'configure' with an appropriate default when initializing.
-}
execute :: Program None α -> IO ()
execute program = do
    context <- configure "" None (simple [])
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

    -- force UTF-8 working around bad VMs
    setLocaleEncoding utf8

    let quit = exitSemaphoreFrom context
        level = verbosityLevelFrom context
        out = outputChannelFrom context
        log = loggerChannelFrom context

    -- set up standard output
    o <- async $ do
        Safe.catchesAsync
            (processStandardOutput out)
            (collapseHandlers)

    -- set up debug logger
    l <- async $ do
        Safe.catchesAsync
            (processDebugMessages log)
            (collapseHandlers)

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

    -- drain message queues. Allow 0.1 seconds, then timeout, in case
    -- something has gone wrong and queues don't empty.
    race_
        (do
            atomically $ do
                done2 <- isEmptyTQueue log
                check done2

                done1 <- isEmptyTQueue out
                check done1)
        (do
            threadDelay 100000
            putStrLn "error: Timeout")

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
-- putting to the quit MVar initiates the cleanup and exit sequence,
-- but throwing the exception also aborts execution and starts unwinding
-- back up the stack.
terminate :: Int -> Program τ α
terminate code =
  let
    exit = case code of
        0 -> ExitSuccess
        _ -> ExitFailure code
  in do
    context <- ask
    let quit = exitSemaphoreFrom context
    liftIO $ do
        putMVar quit exit
        Safe.throw exit

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
Write the supplied @Bytes@ to the given @Handle@. Note that in contrast to
'write' we don't output a trailing newline.

@
    'output' h b
@

Do /not/ use this to output to @stdout@ as that would bypass the mechanism
used by the 'write'*, 'event', and 'debug'* functions to sequence output
correctly. If you wish to write to the terminal use:

@
    'write' ('intoRope' b)
@

(which is not /unsafe/, but will lead to unexpected results if the binary
blob you pass in is other than UTF-8 text).
-}
output :: Handle -> Bytes -> Program τ ()
output handle contents = liftIO (hOutput handle contents)

{-|
Read the (entire) contents of the specified @Handle@.
-}
input :: Handle -> Program τ Bytes
input handle = liftIO (hInput handle)

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
            subProgram context program
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

The command-line parameters are returned in a 'Map', mapping from from the
option or argument name to the supplied value. You can query this map
directly:

@
program = do
    params <- 'getCommandLine'
    let result = 'lookupKeyValue' \"silence\" (paramterValuesFrom params)
    case result of
        'Nothing' -> 'return' ()
        'Just' quiet = case quiet of
            'Value' _ -> 'throw' NotQuiteRight               -- complain that flag doesn't take value
            'Empty'   -> 'write' \"You should be quiet now\"   -- much better
    ...
@

which is pattern matching to answer "was this option specified by the
user?" or "what was the value of this [mandatory] argument?", and then "if
so, did the parameter have a value?"

This is available should you need to differentiate between a @Value@ and an
@Empty@ 'ParameterValue', but for many cases as a convenience you can use
the 'lookupOptionFlag', 'lookupOptionValue', and 'lookupArgument' functions
below (which are just wrappers around a code block like the example shown
here).
-}
getCommandLine :: Program τ (Parameters)
getCommandLine = do
    context <- ask
    return (commandLineFrom context)

{-|
Arguments are mandatory, so by the time your program is running a value
has already been identified. This returns the value for that parameter.
-}
-- this is Maybe because you can inadvertently ask for an unconfigured name
-- this could be fixed with a much stronger Config type, potentially.
lookupArgument :: LongName -> Parameters -> Maybe String
lookupArgument name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            Empty -> error "Invalid State"
            Value value -> Just value

{-|
Look to see if the user supplied a valued option and if so, what its value
was.
-}
-- Should this be more severe if it encounters Empty?
lookupOptionValue :: LongName -> Parameters -> Maybe String
lookupOptionValue name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            Empty -> Nothing
            Value value -> Just value

{-|
Returns @Just True@ if the option is present, and @Nothing@ if it is not.
-}
-- The type is boolean to support a possible future extension of negated
-- arguments.
lookupOptionFlag :: LongName -> Parameters -> Maybe Bool
lookupOptionFlag name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            _ -> Just True        -- nom, nom


{-|
Illegal internal state resulting from what should be unreachable code
or otherwise a programmer error.
-}
invalid :: Program τ α
invalid = error "Invalid State"
