{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module
module Core.Program.Context
    ( 
        Context(..)
      , configure
      , Message(..)
      , Nature(..)
      , Program(..)
      , getConsoleWidth
    ) where

import Chrono.TimeStamp (TimeStamp, getCurrentTimeNanoseconds)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO)
import Control.Exception.Safe (displayException)
import qualified Control.Exception.Safe as Safe (throw)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, LayoutOptions(..), PageWidth(..))
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import System.Console.Terminal.Size (Window(..), size, hSize)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)

import Core.System.External
import Core.Text.Bytes
import Core.Text.Rope
import Core.Text.Utilities
import Core.Program.Arguments

{-|
Internal context for a running program. You access this via actions in the
'Program' monad.
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
data Context = Context {
      programNameFrom :: Rope
    , commandLineFrom :: Parameters
    , exitSemaphoreFrom :: MVar ExitCode
    , startTimeFrom :: TimeStamp
    , terminalWidthFrom :: Int
    , outputChannelFrom :: TChan Rope
    , loggerChannelFrom :: TChan Message
}

data Message = Message TimeStamp Nature Rope (Maybe Rope)

data Nature = Output | Event | Debug

{-|
The type of a top-level Prgoram.

You would use this by writing:

@
module Main where

import "Core.Program"

main :: 'IO' ()
main = 'Core.Program.Execute.execute' program
@

and defining a program that is the top level of your application:

@
program :: 'Program' ()
@

Such actions are combinable; you can sequence them (using bind in
do-notation) or run them in parallel, but basically you should need one
such object at the top of your application.

One of the quirks of Haskell is that it is difficult to refer to code in
the Main module when you've got a number of programs kicking around in a
project each with a @main@ function. So you're best off putting your
top-level 'Program' actions in a separate modules so you can refer to them
from test suites and example snippets.
-}
newtype Program a = Program (ReaderT (MVar Context) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MVar Context))

--
-- This is complicated. The **safe-exceptions** library exports a
-- `throwM` which is not the `throwM` class method from MonadThrow.
-- See https://github.com/fpco/safe-exceptions/issues/31 for
-- discussion. In any event, the re-exports flow back to
-- Control.Monad.Catch from **exceptions** and Control.Exceptions in
-- **base**. In _this_ module, we need to catch everything (including
-- asynchronous exceptions); elsewhere we will use and wrap/export
-- **safe-exceptions**'s variants of the functions.
--
instance MonadThrow Program where
    throwM = liftIO . Safe.throw


{-
    FIXME
    Change to global quit semaphore, reachable anywhere?
-}

instance Semigroup Context where
    (<>) one two = Context {
          programNameFrom = (programNameFrom two)
        , commandLineFrom = (commandLineFrom one)
        , exitSemaphoreFrom = (exitSemaphoreFrom one)
        , startTimeFrom = startTimeFrom one
        , terminalWidthFrom = terminalWidthFrom two
        , outputChannelFrom = outputChannelFrom one
        , loggerChannelFrom = loggerChannelFrom one
        }

{-|
Initialize the programs's execution context. This takes care of various
administrative actions, including setting up output channels, parsing
command-line arguments (according to the supplied configuration), and
putting in place various semaphores for internal program communication.
-}
configure :: Config -> IO Context
configure config = do
    start <- getCurrentTimeNanoseconds

    name <- getProgName
    parameters <- handleCommandLine config
    quit <- newEmptyMVar
    width <- getConsoleWidth
    output <- newTChanIO
    logger <- newTChanIO

    return $! Context {
          programNameFrom = (intoRope name)
        , commandLineFrom = parameters
        , exitSemaphoreFrom = quit
        , startTimeFrom = start
        , terminalWidthFrom = width
        , outputChannelFrom = output
        , loggerChannelFrom = logger
    }

--
-- | Probe the width of the terminal, in characters. If it fails to retrieve,
-- for whatever reason, return a default of 80 characters wide.
--
getConsoleWidth :: IO (Int)
getConsoleWidth = do
    window <- size
    let width =  case window of
            Just (Window _ w) -> w
            Nothing -> 80
    return width

--
-- | Process the command line options and arguments. If an invalid
-- option is encountered or a [mandatory] argument is missing, then
-- the program will terminate here.
--
{-
    We came back here with the error case so we can pass config in to
    buildUsage (otherwise we could have done it all in displayException and
    called that in Core.Program.Arguments). And, returning here lets us set
    up the layout width to match (one off the) actual width of console.
-}
handleCommandLine :: Config -> IO Parameters
handleCommandLine config = do
    argv <- getArgs
    let result = parseCommandLine config argv
    case result of
        Right parameters -> return parameters
        Left e -> case e of
            HelpRequest mode -> do
                width <- getConsoleWidth
                let options = LayoutOptions (AvailablePerLine (width - 1) 1.0)
                let usage = buildUsage config mode
                renderIO stdout (layoutPretty options usage)
                hFlush stdout
                exitWith (ExitFailure 1)
            _ -> do
                putStr "error: "
                putStrLn (displayException e)
                hFlush stdout
                exitWith (ExitFailure 1)

