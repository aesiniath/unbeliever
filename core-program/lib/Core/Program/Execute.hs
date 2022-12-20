{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
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
    ( Program ()

      -- * Running programs
    , configure
    , execute
    , executeWith

      -- * Exiting a program
    , terminate

      -- * Accessing program context
    , getCommandLine
    , queryCommandName
    , queryOptionFlag
    , queryOptionValue
    , queryOptionValue'
    , queryArgument
    , queryRemaining
    , queryEnvironmentValue
    , getProgramName
    , setProgramName
    , getVerbosityLevel
    , setVerbosityLevel
    , getConsoleWidth
    , getApplicationState
    , setApplicationState

      -- * Useful actions
    , outputEntire
    , inputEntire
    , execProcess
    , sleepThread
    , resetTimer
    , trap_

      -- * Exception handling
    , catch
    , throw
    , try

      -- * Internals
    , Context
    , None (..)
    , isNone
    , unProgram
    , invalid
    , Boom (..)
    , loopForever
    , lookupOptionFlag
    , lookupOptionValue
    , lookupArgument
    , lookupEnvironmentValue
    ) where

import Control.Concurrent
    ( forkFinally
    , forkIO
    , killThread
    , myThreadId
    , threadDelay
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newEmptyMVar
    , putMVar
    , readMVar
    , tryPutMVar
    )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
    ( TQueue
    , readTQueue
    , tryReadTQueue
    , unGetTQueue
    , writeTQueue
    )
import Control.Concurrent.STM.TVar
    ( readTVarIO
    )
import Control.Exception qualified as Base (throwIO)
import Control.Exception.Safe qualified as Safe
    ( catch
    , throw
    )
import Control.Monad
    ( forM_
    , forever
    , void
    , when
    )
import Control.Monad.Reader.Class (MonadReader (ask))
import Core.Data.Clock
import Core.Data.Structures
import Core.Encoding.External
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Exceptions
import Core.Program.Logging
import Core.Program.Signal
import Core.System.Base
    ( Exception
    , Handle
    , SomeException
    , displayException
    , hFlush
    , liftIO
    , stdout
    )
import Core.Text.Bytes
import Core.Text.Rope
import Data.ByteString qualified as B (hPut)
import Data.ByteString.Char8 qualified as C (singleton)
import Data.List qualified as List (intersperse)
import GHC.Conc (getNumProcessors, numCapabilities, setNumCapabilities)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory
    ( findExecutable
    )
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, readProcess, setStdin)
import Prelude hiding (log)

{- |
Trap any exceptions coming out of the given Program action, and discard them.
The one and only time you want this is inside an endless loop:

@
    'Conrol.Monad.forever' $ do
        'trap_'
            ( 'bracket'
                obtainResource
                releaseResource
                useResource
            )
@

This function really will swollow expcetions, which means that you'd better
have handled any synchronous checked errors already with a 'catch' and/or have
released resources with 'bracket' or 'finally' as shown above.

A warning level message will be sent to the log channel indicating that an
uncaught exception was trapped along with a debug level message showing the
exception text, if any.

@since 0.2.11
-}
trap_ :: Program τ α -> Program τ ()
trap_ action =
    Safe.catch
        (void action)
        ( \(e :: SomeException) ->
            let text = intoRope (displayException e)
            in  do
                    warn "Trapped uncaught exception"
                    debug "e" text
        )

{- |
Embelish a program with useful behaviours. See module header
"Core.Program.Execute" for a detailed description. Internally this function
calls 'configure' with an appropriate default when initializing.
-}
execute :: Program None α -> IO ()
execute program = do
    context <- configure "" None (simpleConfig [])
    executeActual context program

{- |
Embelish a program with useful behaviours, supplying a configuration
for command-line options & argument parsing and an initial value for
the top-level application state, if appropriate.
-}
executeWith :: Context τ -> Program τ α -> IO ()
executeWith = executeActual

executeActual :: Context τ -> Program τ α -> IO ()
executeActual context0 program = do
    -- command line +RTS -Nn -RTS value
    when (numCapabilities == 1) (getNumProcessors >>= setNumCapabilities)

    -- force UTF-8 working around bad VMs
    setLocaleEncoding utf8

    context1 <- handleCommandLine context0
    context <- handleTelemetryChoice context1

    level <- handleVerbosityLevel context

    let quit = exitSemaphoreFrom context
        out = outputChannelFrom context
        tel = telemetryChannelFrom context
        forwarder = telemetryForwarderFrom context

    -- set up signal handlers
    _ <- forkIO $ do
        setupSignalHandlers quit level

    -- set up standard output
    vo <- newEmptyMVar
    _ <-
        forkFinally
            (processStandardOutput out)
            (\_ -> putMVar vo ())

    -- set up debug logger
    vl <- newEmptyMVar
    _ <-
        forkFinally
            (processTelemetryMessages forwarder level out tel)
            (\_ -> putMVar vl ())

    -- run actual program, ensuring to grab any otherwise uncaught exceptions.
    t1 <- forkIO $ do
        Safe.catch
            ( do
                --
                -- execute actual "main". Note that we're not passing the
                -- Scope into the program's Context; it stays the default
                -- Nothing because the outer Scope is none of the
                -- program's business and we absolutely don't want an
                -- awaitAll to sit there and block on our machinery
                -- threads.
                --
                -- We use tryPutMVar here (rather than putMVar) because we
                -- might already be on the way out and need to not block.
                --
                _ <- subProgram context program
                _ <- tryPutMVar quit ExitSuccess
                pure ()
            )
            ( \(e :: SomeException) -> do
                let text = intoRope (displayException e)
                subProgram context $ do
                    setVerbosityLevel Debug
                    critical text
                _ <- tryPutMVar quit (ExitFailure 127)
                pure ()
            )

    -- wait for indication to terminate
    code <- readMVar quit

    -- kill main thread
    killThread t1

    -- instruct handlers to finish, and wait for the message queues to
    -- drain. Allow 10 seconds, then timeout, in case something has gone
    -- wrong and queues don't empty.

    _ <- forkIO $ do
        threadDelay 10000000
        putStrLn "error: Timeout"
        Safe.throw (ExitFailure 99)

    _ <- forkIO $ do
        let scope = currentScopeFrom context
        pointers <- readTVarIO scope
        forM_ pointers killThread

    atomically $ do
        writeTQueue tel Nothing

    readMVar vl

    atomically $ do
        writeTQueue out Nothing

    readMVar vo

    hFlush stdout

    -- exiting this way avoids "Exception: ExitSuccess" noise in GHCi
    if code == ExitSuccess
        then return ()
        else (Base.throwIO code)

processStandardOutput :: TQueue (Maybe Rope) -> IO ()
processStandardOutput out =
    loop
  where
    loop :: IO ()
    loop = do
        probable <- atomically $ do
            readTQueue out

        case probable of
            Nothing -> pure ()
            Just text -> do
                hWrite stdout text
                B.hPut stdout (C.singleton '\n')
                hFlush stdout
                loop

--
-- I'm embarrased how long it took to get here. At one point we were firing
-- off an Async.race of two threads for every item coming down the queue. And
-- you know what? That didn't work either. After all of that, realized that
-- the technique used   by **io-streams** to just pass along a stream of Maybes,
-- with Nothing signalling end-of-stream is exactly good enough for our needs.
--
processTelemetryMessages :: Maybe Forwarder -> MVar Verbosity -> TQueue (Maybe Rope) -> TQueue (Maybe Datum) -> IO ()
processTelemetryMessages Nothing _ _ tel = do
    ignoreForever tel
  where
    ignoreForever queue = do
        possibleItem <- atomically $ do
            readTQueue queue -- blocks
        case possibleItem of
            -- time to shutdown
            Nothing -> pure ()
            -- otherwise igonore
            Just _ -> do
                ignoreForever queue
processTelemetryMessages (Just processor) v out tel = do
    loopForever action v out tel
  where
    action = telemetryHandlerFrom processor

loopForever :: ([a] -> IO ()) -> MVar Verbosity -> TQueue (Maybe Rope) -> TQueue (Maybe a) -> IO ()
loopForever action v out queue = do
    -- block waiting for an item
    possibleItems <- atomically $ do
        cycleOverQueue 0 []

    case possibleItems of
        -- we're done!
        Nothing -> pure ()
        -- handle it and loop
        Just items -> do
            start <- getCurrentTimeNanoseconds
            Safe.catch
                ( do
                    action (reverse items)
                    reportStatus start (length items)
                )
                ( \(e :: SomeException) -> do
                    reportProblem start e
                )
            loopForever action v out queue
  where
    cycleOverQueue !count items =
        if count >= (1024 :: Int)
            then pure (Just items)
            else cycleOverQueue' count items

    cycleOverQueue' !count items =
        case items of
            [] -> do
                possibleItem <- readTQueue queue -- blocks
                case possibleItem of
                    -- we're finished! time to shutdown
                    Nothing -> pure Nothing
                    -- otherwise start accumulating
                    Just item -> do
                        cycleOverQueue 1 (item : [])
            _ -> do
                pending <- tryReadTQueue queue -- doesn't block
                case pending of
                    -- nothing left in the queue
                    Nothing -> pure (Just items)
                    -- otherwise we get one of our Maybe Datum, and consider it
                    Just possibleItem -> do
                        case possibleItem of
                            -- oh, time to stop! We put the Nothing back into
                            -- the queue, then let the accumulated items get
                            -- processed. The next loop will read the
                            -- Nothing and shutdown.
                            Nothing -> do
                                unGetTQueue queue Nothing
                                pure (Just items)
                            -- continue accumulating!
                            Just item -> do
                                cycleOverQueue (count + 1) (item : items)

    reportStatus start num = do
        level <- readMVar v
        when (isInternal level) $ do
            now <- getCurrentTimeNanoseconds
            let desc = case num of
                    1 -> "1 event"
                    _ -> intoRope (show num) <> " events"
                message =
                    formatLogMessage
                        start
                        now
                        True
                        SeverityInternal
                        ("Sent " <> desc)
            atomically $ do
                writeTQueue out (Just message)

    reportProblem start e = do
        level <- readMVar v
        when (isEvent level) $ do
            now <- getCurrentTimeNanoseconds
            let message =
                    formatLogMessage
                        start
                        now
                        True
                        SeverityWarn
                        ("Sending telemetry failed (Exception: " <> intoRope (show e) <> "); Restarting exporter.")
            atomically $ do
                writeTQueue out (Just message)

{- |
Safely exit the program with the supplied exit code. Current output and debug
queues will be flushed, and then the process will terminate. This function
does not return.
-}

-- putting to the quit MVar initiates the cleanup and exit sequence, but
-- throwing the asynchronous exception to self also aborts execution and
-- starts unwinding back up the stack.
--
-- forever is used here to get an IO α as the return type.
terminate :: Int -> Program τ α
terminate code = do
    context <- ask
    let quit = exitSemaphoreFrom context

    let exit = case code of
            0 -> ExitSuccess
            _ -> ExitFailure code

    liftIO $ do
        putMVar quit exit
        self <- myThreadId
        killThread self
        forever $ do
            threadDelay maxBound

-- undocumented
getVerbosityLevel :: Program τ Verbosity
getVerbosityLevel = do
    context <- ask
    liftIO $ do
        level <- readMVar (verbosityLevelFrom context)
        return level

{- |
Change the verbosity level of the program's logging output. This changes
whether 'info' and the 'debug' family of functions emit to the logging
stream; they do /not/ affect 'write'ing to the terminal on the standard
output stream.
-}
setVerbosityLevel :: Verbosity -> Program τ ()
setVerbosityLevel level = do
    context <- ask
    liftIO $ do
        let v = verbosityLevelFrom context
        modifyMVar_ v (\_ -> pure level)

{- |
Override the program name used for logging, etc. At least, that was the
idea. Nothing makes use of this at the moment. @:/@
-}
setProgramName :: Rope -> Program τ ()
setProgramName name = do
    context <- ask
    liftIO $ do
        let v = programNameFrom context
        modifyMVar_ v (\_ -> pure name)

{- |
Get the program name as invoked from the command-line (or as overridden by
'setProgramName').
-}
getProgramName :: Program τ Rope
getProgramName = do
    context <- ask
    liftIO $ do
        let v = programNameFrom context
        readMVar v

{- |
Retreive the current terminal's width, in characters.

If you are outputting an object with a 'Core.Text.Untilities.Render'
instance then you may not need this; you can instead use 'writeR' which is
aware of the width of your terminal and will reflow (in as much as the
underlying type's @Render@ instance lets it).
-}
getConsoleWidth :: Program τ Int
getConsoleWidth = do
    context <- ask
    let width = terminalWidthFrom context
    return width

{- |
Get the user supplied application state as originally supplied to
'configure' and modified subsequntly by replacement with
'setApplicationState'.

@
    settings <- 'getApplicationState'
@
-}
getApplicationState :: Program τ τ
getApplicationState = do
    context <- ask
    liftIO $ do
        let v = applicationDataFrom context
        readMVar v

{- |
Update the user supplied top-level application state.

@
    let settings' = settings { answer = 42 }
    'setApplicationState' settings'
@
-}
setApplicationState :: τ -> Program τ ()
setApplicationState user = do
    context <- ask
    liftIO $ do
        let v = applicationDataFrom context
        modifyMVar_ v (\_ -> pure user)

{- |
Write the supplied @Bytes@ to the given @Handle@. Note that in contrast to
'write' we don't output a trailing newline.

@
    'outputEntire' h b
@

Do /not/ use this to output to @stdout@ as that would bypass the mechanism
used by the 'write'*, 'info', and 'debug'* functions to sequence output
correctly. If you wish to write to the terminal use:

@
    'write' ('intoRope' b)
@

(which is not /unsafe/, but will lead to unexpected results if the binary
blob you pass in is other than UTF-8 text).
-}
outputEntire :: Handle -> Bytes -> Program τ ()
outputEntire handle contents = liftIO (hOutput handle contents)

{- |
Read the (entire) contents of the specified @Handle@.
-}
inputEntire :: Handle -> Program τ Bytes
inputEntire handle = liftIO (hInput handle)

data ProcessProblem
    = CommandNotFound Rope
    deriving (Show)

instance Exception ProcessProblem

{- |
Execute an external child process and wait for its output and result. The
command is specified first and and subsequent arguments as elements of the
list. This helper then logs the command being executed to the debug output,
which can be useful when you're trying to find out what exactly what program
is being invoked.

Keep in mind that this isn't invoking a shell; arguments and their values have
to be enumerated separately:

@
    'execProcess' [\"\/usr\/bin\/ssh\", \"-l\", \"admin\", \"203.0.113.42\", \"\\\'remote command here\\\'\"]
@

having to write out the individual options and arguments and deal with
escaping is a bit of an annoyance but that's /execvp(3)/ for you.

The return tuple is the exit code from the child process, its entire @stdout@
and its entire @stderr@, if any. Note that this is not a streaming interface,
so if you're doing something that returns huge amounts of output you'll want
to use something like __io-streams__ instead.

(this wraps __typed-process__'s 'readProcess')
-}
execProcess :: [Rope] -> Program τ (ExitCode, Rope, Rope)
execProcess [] = error "No command provided"
execProcess (cmd : args) =
    let cmd' = fromRope cmd
        args' = fmap fromRope args
        task = proc cmd' args'
        task1 = setStdin nullStream task
        command = mconcat (List.intersperse (singletonRope ' ') (cmd : args))
    in  do
            debug "command" command

            probe <- liftIO $ do
                findExecutable cmd'
            case probe of
                Nothing -> do
                    Safe.throw (CommandNotFound cmd)
                Just _ -> do
                    (exit, out, err) <- liftIO $ do
                        readProcess task1

                    pure (exit, intoRope out, intoRope err)

{- |
Reset the start time (used to calculate durations shown in event- and
debug-level logging) held in the @Context@ to zero. This is useful if you want
to see the elapsed time taken by a specific worker rather than seeing log
entries relative to the program start time which is the default.

If you want to start time held on your main program thread to maintain a count
of the total elapsed program time, then fork a new thread for your worker and
reset the timer there.

@
    'Core.Program.Threads.forkThread' $ do
        'resetTimer'
        ...
@

then times output in the log messages will be relative to that call to
'resetTimer', not the program start.

@since 0.2.7
-}
resetTimer :: Program τ ()
resetTimer = do
    context <- ask

    liftIO $ do
        start <- getCurrentTimeNanoseconds
        let v = startTimeFrom context
        modifyMVar_ v (\_ -> pure start)

{- |
Pause the current thread for the given number of seconds. For
example, to delay a second and a half, do:

@
    'sleepThread' 1.5
@

(this wraps __base__'s 'threadDelay')
-}

--
-- FIXME is this the right type, given we want to avoid type default warnings?
--
sleepThread :: Rational -> Program τ ()
sleepThread seconds =
    let us = floor (toRational (seconds * 1e6))
    in  liftIO $ threadDelay us

{- |
Retrieve the values of parameters parsed from options and arguments supplied
by the user on the command-line.

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
            'Value' _ -> 'throw' NotQuiteRight                 -- complain that flag doesn't take value
            'Empty'   -> 'write' \"You should be quiet now\"   -- much better
    ...
@

which is pattern matching to answer "was this option specified by the user?"
or "what was the value of this [mandatory] argument?", and then "if so, did
the parameter have a value?"

This is available should you need to differentiate between a @Value@ and an
@Empty@ 'ParameterValue', but for many cases as a convenience you can use the
'queryOptionFlag', 'queryOptionValue', and 'queryArgument' functions below.
-}
getCommandLine :: Program τ (Parameters)
getCommandLine = do
    context <- ask
    return (commandLineFrom context)

{- |
Arguments are mandatory, so by the time your program is running a value
has already been identified. This retreives the value for that parameter.

@
program = do
    file <- 'queryArgument' \"filename\"
    ...
@

@since 0.2.7
-}
queryArgument :: LongName -> Program τ Rope
queryArgument name = do
    context <- ask
    let params = commandLineFrom context
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> error "Attempted lookup of unconfigured argument"
        Just argument -> case argument of
            Empty -> error "Invalid State"
            Value value -> pure (intoRope value)

lookupArgument :: LongName -> Parameters -> Maybe String
lookupArgument name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            Empty -> error "Invalid State"
            Value value -> Just value
{-# DEPRECATED lookupArgument "Use queryArgument instead" #-}

{- |
In other applications, you want to gather up the remaining arguments on the
command-line. You need to have specified 'Remaining' in the configuration.

@
program = do
    files \<- 'queryRemaining'
    ...
@

@since 0.3.5
-}
queryRemaining :: Program τ [Rope]
queryRemaining = do
    context <- ask
    let params = commandLineFrom context
    let remaining = remainingArgumentsFrom params
    pure (fmap intoRope remaining)

{- |
Look to see if the user supplied a valued option and if so, what its value
was. Use of the @LambdaCase@ extension makes accessing the option (and
specifying a default if it is absent) reasonably nice:

@
program = do
    region \<- 'queryOptionValue' \"region\" '>>=' \\case
        'Nothing' -> 'pure' \"us-west-2\" -- Oregon, not a bad default
        'Just' value -> 'pure' value
@

If you require something other than the text value as entered by the user
you'll need to do something to parse the returned value and convert it to an
appropriate type  See 'queryOptionValue'' for an alternative that does this
automatically in many common cases, i.e. for options that take numberic
values.

@since 0.3.5
-}
queryOptionValue :: LongName -> Program τ (Maybe Rope)
queryOptionValue name = do
    context <- ask
    let params = commandLineFrom context
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> pure Nothing
        Just argument -> case argument of
            Empty -> pure (Just emptyRope)
            Value value -> pure (Just (intoRope value))

lookupOptionValue :: LongName -> Parameters -> Maybe String
lookupOptionValue name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            Empty -> Nothing
            Value value -> Just value
{-# DEPRECATED lookupOptionValue "Use queryOptionValue instead" #-}

data QueryParameterError
    = OptionValueMissing LongName
    | UnableParseValue LongName
    deriving (Show)

instance Exception QueryParameterError where
    displayException e = case e of
        OptionValueMissing (LongName name) -> "Option --" ++ name ++ " specified but without a value."
        UnableParseValue (LongName name) -> "Unable to parse the value supplied to --" ++ name ++ "."

{- |
Look to see if the user supplied a valued option and if so, what its value
was. This covers the common case of wanting to read a numeric argument from an
option:

@
program = do
    count \<- 'queryOptionValue'' \"count\" '>>=' \\case
        'Nothing' -> 'pure' (0 :: 'Int')
        'Just' value -> 'pure' value
    ...
@

The return type of this function has the same semantics as 'queryOptionValue':
if the option is absent you get 'Nothing' back (and in the example above we
specify a default in that case) and 'Just' if a value is present. Unlike the
original function, however, here we assume success in reading the value! If
the value is unable to be parsed into the nominated Haskell type using
'parseExternal' then an exception with an appropriate error message will be
thrown­—which is what you want if the user specifies something that can't be
parsed.

Note that the return type is polymorphic so you'll need to ensure the concrete
type you actually want is specified either via type inference or by adding a
type annotation somewhere.

@since 0.5.1
-}
queryOptionValue' :: Externalize ξ => LongName -> Program τ (Maybe ξ)
queryOptionValue' name = do
    context <- ask
    let params = commandLineFrom context
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> pure Nothing
        Just parameter -> case parameter of
            Empty -> throw (OptionValueMissing name)
            Value value -> case parseExternal (packRope value) of
                Nothing -> throw (UnableParseValue name)
                Just actual -> pure (Just actual)

{- |
Returns @True@ if the option is present, and @False@ if it is not.

@
program = do
    overwrite \<- 'queryOptionFlag' \"overwrite\"
    ...
@

@since 0.3.5
-}
queryOptionFlag :: LongName -> Program τ Bool
queryOptionFlag name = do
    context <- ask
    let params = commandLineFrom context
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> pure False
        Just _ -> pure True

lookupOptionFlag :: LongName -> Parameters -> Maybe Bool
lookupOptionFlag name params =
    case lookupKeyValue name (parameterValuesFrom params) of
        Nothing -> Nothing
        Just argument -> case argument of
            _ -> Just True -- nom, nom
{-# DEPRECATED lookupOptionFlag "Use queryOptionFlag instead" #-}

{- |
Look to see if the user supplied the named environment variable and if so,
return what its value was.

@since 0.3.5
-}
queryEnvironmentValue :: LongName -> Program τ (Maybe Rope)
queryEnvironmentValue name = do
    context <- ask
    let params = commandLineFrom context
    case lookupKeyValue name (environmentValuesFrom params) of
        Nothing -> error "Attempted lookup of unconfigured environment variable"
        Just param -> case param of
            Empty -> pure Nothing
            Value str -> pure (Just (intoRope str))

lookupEnvironmentValue :: LongName -> Parameters -> Maybe String
lookupEnvironmentValue name params =
    case lookupKeyValue name (environmentValuesFrom params) of
        Nothing -> Nothing
        Just param -> case param of
            Empty -> Nothing
            Value str -> Just str
{-# DEPRECATED lookupEnvironmentValue "Use queryEnvironment instead" #-}

{- |
Retreive the sub-command mode selected by the user. This assumes your program
was set up to take sub-commands via 'complexConfig'.

@
    mode <- queryCommandName
@

@since 0.3.5
-}
queryCommandName :: Program τ Rope
queryCommandName = do
    context <- ask
    let params = commandLineFrom context
    case commandNameFrom params of
        Just (LongName name) -> pure (intoRope name)
        Nothing -> error "Attempted lookup of command but not a Complex Config"

{- |
Illegal internal state resulting from what should be unreachable code or
otherwise a programmer error.
-}
invalid :: Program τ α
invalid = error "Invalid State"
