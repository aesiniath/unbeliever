{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module, hidden from Haddock
module Core.Program.Context (
    Datum (..),
    emptyDatum,
    Trace (..),
    Context (..),
    handleCommandLine,
    handleVerbosityLevel,
    Exporter (..),
    emptyExporter,
    None (..),
    isNone,
    configure,
    Verbosity (..),
    Program (..),
    unProgram,
    getContext,
    fmapContext,
    subProgram,
) where

import Chrono.TimeStamp (TimeStamp, getCurrentTimeNanoseconds)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import qualified Control.Exception.Safe as Safe (throw)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow (throwM))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Core.Data.Structures
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Metadata
import Core.System.Base hiding (catch, throw)
import Core.Text.Rope
import Data.Foldable (foldrM)
import Data.Int (Int64)
import Prettyprinter (LayoutOptions (..), PageWidth (..), layoutPretty)
import Prettyprinter.Render.Text (renderIO)
import qualified System.Console.Terminal.Size as Terminal (Window (..), size)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import Prelude hiding (log)

data Datum = Datum
    { datumIdentifierFrom :: Rope
    , datumNameFrom :: Rope
    , datumTimeFrom :: TimeStamp
    , parentTraceFrom :: Maybe Trace
    , parentSpanFrom :: Maybe Datum
    , datumDuration :: Maybe Int64
    , attachedMetadata :: MVar (Map JsonKey JsonValue)
    }

emptyDatum :: Datum
emptyDatum =
    Datum
        { datumIdentifierFrom = emptyRope
        , datumNameFrom = emptyRope
        , datumTimeFrom = 0
        , parentTraceFrom = Nothing
        , parentSpanFrom = Nothing
        , datumDuration = Nothing
        , attachedMetadata = emptyMap
        }

data Trace = Trace
    { traceIdentifierFrom :: Rope
    }

{- |
Implementation of a forwarder for structured logging of the telemetry channel.
-}
data Exporter = Exporter

emptyExporter :: Exporter
emptyExporter = Exporter

{- |
Internal context for a running program. You access this via actions in the
'Program' monad. The principal item here is the user-supplied top-level
application data of type @τ@ which can be retrieved with
'Core.Program.Execute.getApplicationState' and updated with
'Core.Program.Execute.setApplicationState'.
-}

--
-- The fieldNameFrom idiom is an experiment. Looks very strange,
-- certainly, here in the record type definition and when setting
-- fields, but for the common case of getting a value out of the
-- record, a call like
--
--     fieldNameFrom context
--
-- isn't bad at all, and no worse than the leading underscore
-- convention.
--
--     _fieldName context
--
-- (I would argue better, since _ is already so overloaded as the
-- wildcard symbol in Haskell). Either way, the point is to avoid a
-- bare fieldName because so often you have want to be able to use
-- that field name as a local variable name.
--
data Context τ = Context
    { programNameFrom :: MVar Rope
    , versionFrom :: Version
    , initialConfigFrom :: Config
    , commandLineFrom :: Parameters
    , exitSemaphoreFrom :: MVar ExitCode
    , startTimeFrom :: MVar TimeStamp
    , terminalWidthFrom :: Int
    , verbosityLevelFrom :: MVar Verbosity
    , outputChannelFrom :: TQueue Rope
    , telemetryChannelFrom :: TQueue Datum
    , telemetryExporterFrom :: Exporter
    , currentDatumFrom :: MVar Datum
    , applicationDataFrom :: MVar τ
    }

-- I would happily accept critique as to whether this is safe or not. I think
-- so? The only way to get to the underlying top-level application data is
-- through 'getApplicationState' which is in Program monad so the fact that it
-- is implemented within an MVar should be irrelevant.
instance Functor Context where
    fmap f = unsafePerformIO . fmapContext f

{- |
Map a function over the underlying user-data inside the 'Context', changing
it from type@τ1@ to @τ2@.
-}
fmapContext :: (τ1 -> τ2) -> Context τ1 -> IO (Context τ2)
fmapContext f context = do
    state <- readMVar (applicationDataFrom context)
    let state' = f state
    u <- newMVar state'
    return (context{applicationDataFrom = u})

{- |
A 'Program' with no user-supplied state to be threaded throughout the
computation.

The "Core.Program.Execute" framework makes your top-level application state
available at the outer level of your process. While this is a feature that
most substantial programs rely on, it is /not/ needed for many simple tasks or
when first starting out what will become a larger project.

This is effectively the unit type, but this alias is here to clearly signal a
user-data type is not a part of the program semantics.
-}

-- Bids are open for a better name for this
data None = None
    deriving (Show, Eq)

isNone :: None -> Bool
isNone _ = True

{- |
The verbosity level of the output logging subsystem. You can override the
level specified on the command-line by calling
'Core.Program.Execute.setVerbosityLevel' from within the 'Program' monad.
-}
data Verbosity
    = Output
    | Event
    | -- | @since 0.2.12
      Verbose
    | Debug
    deriving (Show)

{-# DEPRECATED Event "Use Verbose instead" #-}

{- |
The type of a top-level program.

You would use this by writing:

@
module Main where

import "Core.Program"

main :: 'IO' ()
main = 'Core.Program.Execute.execute' program
@

and defining a program that is the top level of your application:

@
program :: 'Program' 'None' ()
@

Such actions are combinable; you can sequence them (using bind in do-notation)
or run them in parallel, but basically you should need one such object at the
top of your application.

/Type variables/

A 'Program' has a user-supplied application state and a return type.

The first type variable, @τ@, is your application's state. This is an object
that will be threaded through the computation and made available to your code
in the 'Program' monad. While this is a common requirement of the outer code
layer in large programs, it is often /not/ necessary in small programs or when
starting new projects. You can mark that there is no top-level application
state required using 'None' and easily change it later if your needs evolve.

The return type, @α@, is usually unit as this effectively being called
directly from @main@ and Haskell programs have type @'IO' ()@. That is, they
don't return anything; I/O having already happened as side effects.

/Programs in separate modules/

One of the quirks of Haskell is that it is difficult to refer to code in the
Main module when you've got a number of programs kicking around in a project
each with a @main@ function. One way of dealing with this is to put your
top-level 'Program' actions in a separate modules so you can refer to them
from test suites and example snippets.

/Interoperating with the rest of the Haskell ecosystem/

The 'Program' monad is a wrapper over 'IO'; at any point when you need to move
to another package's entry point, just use 'liftIO'. It's re-exported by
"Core.System.Base" for your convenience. Later, you might be interested in
unlifting back to Program; see "Core.Program.Unlift".
-}
newtype Program τ α = Program (ReaderT (Context τ) IO α)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Context τ))

unProgram :: Program τ α -> ReaderT (Context τ) IO α
unProgram (Program r) = r

{- |
Get the internal @Context@ of the running @Program@. There is ordinarily no
reason to use this; to access your top-level application data @τ@ within the
@Context@ use 'Core.Program.Execute.getApplicationState'.
-}
getContext :: Program τ (Context τ)
getContext = do
    context <- ask
    return context

{- |
Run a subprogram from within a lifted @IO@ block.
-}
subProgram :: Context τ -> Program τ α -> IO α
subProgram context (Program r) = do
    runReaderT r context

{-
This is complicated. The **safe-exceptions** library exports a `throwM` which
is not the `throwM` class method from MonadThrow. See
https://github.com/fpco/safe-exceptions/issues/31 for discussion. In any
event, the re-exports flow back to Control.Monad.Catch from **exceptions** and
Control.Exceptions in **base**. In the execute actions, we need to catch
everything (including asynchronous exceptions); elsewhere we will use and
wrap/export **safe-exceptions**'s variants of the functions.
-}
instance MonadThrow (Program τ) where
    throwM = liftIO . Safe.throw

deriving instance MonadCatch (Program τ)

deriving instance MonadMask (Program t)

{- |
Initialize the programs's execution context. This takes care of various
administrative actions, including setting up output channels, parsing
command-line arguments (according to the supplied configuration), and putting
in place various semaphores for internal program communication. See
"Core.Program.Arguments" for details.

This is also where you specify the initial {blank, empty, default) value for
the top-level user-defined application state, if you have one. Specify 'None'
if you aren't using this feature.
-}
configure :: Version -> τ -> Config -> IO (Context τ)
configure version t config = do
    start <- getCurrentTimeNanoseconds

    arg0 <- getProgName
    n <- newMVar (intoRope arg0)
    q <- newEmptyMVar
    i <- newMVar start
    columns <- getConsoleWidth
    level <- newEmptyMVar
    out <- newTQueueIO
    tel <- newTQueueIO
    s <- newEmptyMVar

    u <- newMVar t

    return
        $! Context
            { programNameFrom = n
            , versionFrom = version
            , initialConfigFrom = config
            , commandLineFrom = emptyParameters -- will be filled in handleCommandLine
            , exitSemaphoreFrom = q
            , startTimeFrom = i
            , terminalWidthFrom = columns
            , verbosityLevelFrom = level -- will be filled in handleVerbosityLevel
            , outputChannelFrom = out
            , telemetryChannelFrom = tel
            , telemetryExporterFrom = emptyExporter
            , currentDatumFrom = s
            , applicationDataFrom = u
            }

--

{- |
Probe the width of the terminal, in characters. If it fails to retrieve, for
whatever reason, return a default of 80 characters wide.
-}
getConsoleWidth :: IO (Int)
getConsoleWidth = do
    window <- Terminal.size
    let columns = case window of
            Just (Terminal.Window _ w) -> w
            Nothing -> 80
    return columns

--

{- |
Process the command line options and arguments. If an invalid option is
encountered or a [mandatory] argument is missing, then the program will
terminate here.
-}

{-
    We came back here with the error case so we can pass config in to
    buildUsage (otherwise we could have done it all in displayException and
    called that in Core.Program.Arguments). And, returning here lets us set
    up the layout width to match (one off the) actual width of console.
-}
handleCommandLine :: Context τ -> IO (Context τ)
handleCommandLine context = do
    argv <- getArgs

    let config = initialConfigFrom context
        version = versionFrom context
        result = parseCommandLine config argv

    case result of
        Right parameters -> do
            pairs <- lookupEnvironmentVariables config parameters
            let params = parameters{environmentValuesFrom = pairs}
            -- update the result of all this and return in
            let context' = context{commandLineFrom = params}
            pure context'
        Left e -> case e of
            HelpRequest mode -> do
                render (buildUsage config mode)
                exitWith (ExitFailure 1)
            VersionRequest -> do
                render (buildVersion version)
                exitWith (ExitFailure 1)
            _ -> do
                putStr "error: "
                putStrLn (displayException e)
                hFlush stdout
                exitWith (ExitFailure 1)
  where
    render message = do
        columns <- getConsoleWidth
        let options = LayoutOptions (AvailablePerLine (columns - 1) 1.0)
        renderIO stdout (layoutPretty options message)
        hFlush stdout

lookupEnvironmentVariables :: Config -> Parameters -> IO (Map LongName ParameterValue)
lookupEnvironmentVariables config params = do
    let mode = commandNameFrom params
    let valids = extractValidEnvironments mode config

    result <- foldrM f emptyMap valids
    return result
  where
    f :: LongName -> (Map LongName ParameterValue) -> IO (Map LongName ParameterValue)
    f name@(LongName var) acc = do
        result <- lookupEnv var
        return $ case result of
            Just value -> insertKeyValue name (Value value) acc
            Nothing -> acc

handleVerbosityLevel :: Context τ -> IO (MVar Verbosity)
handleVerbosityLevel context = do
    let params = commandLineFrom context
        level = verbosityLevelFrom context
        result = queryVerbosityLevel params
    case result of
        Left exit -> do
            putStrLn "error: To set logging level use --verbose or --debug; neither take a value."
            hFlush stdout
            exitWith exit
        Right verbosity -> do
            putMVar level verbosity
            pure level

queryVerbosityLevel :: Parameters -> Either ExitCode Verbosity
queryVerbosityLevel params =
    let debug = lookupKeyValue "debug" (parameterValuesFrom params)
        verbose = lookupKeyValue "verbose" (parameterValuesFrom params)
     in case debug of
            Just value -> case value of
                Empty -> Right Debug
                Value _ -> Left (ExitFailure 2)
            Nothing -> case verbose of
                Just value -> case value of
                    Empty -> Right Verbose
                    Value _ -> Left (ExitFailure 2)
                Nothing -> Right Output
