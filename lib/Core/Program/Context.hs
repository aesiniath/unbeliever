{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Program.Context
    ( 
        Context(..)
      , configure
      , Message(..)
      , Nature(..)
      , getConsoleWidth
    ) where

import Chrono.TimeStamp (TimeStamp, getCurrentTimeNanoseconds)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO)
import Control.Exception.Safe (displayException)
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, LayoutOptions(..), PageWidth(..))
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import System.Console.Terminal.Size (Window(..), size, hSize)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)

import Core.Text
import Core.System
import Core.Render
import Core.Program.Arguments

{-
    The fieldNameFrom idiom is an experiment. Looks very strange,
    certainly, here in the record type definition and when setting
    fields, but for the common case of getting a value out of the
    record, a call like

        fieldNameFrom context

    isn't bad at all, and no worse than the leading underscore
    convention.

        _fieldName context

     (I would argue better, since _ is already so overloaded as the
     wildcard symbol in Haskell). Either way, the point is to avoid a
     bare fieldName because so often you have want to be able to use
     that field name as a local variable name.
-}
data Context = Context {
      programNameFrom :: Text
    , commandLineFrom :: Parameters
    , exitSemaphoreFrom :: MVar ExitCode
    , startTimeFrom :: TimeStamp
    , terminalWidthFrom :: Int
    , outputChannelFrom :: TChan Text
    , loggerChannelFrom :: TChan Message
}

data Message = Message TimeStamp Nature Text (Maybe Text)

data Nature = Output | Event | Debug

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
          programNameFrom = (intoText name)
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

