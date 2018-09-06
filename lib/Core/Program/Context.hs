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
import Control.Exception.Safe (throwString)
import System.Console.Terminal.Size (Window(..), size, hSize)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode)

import Core.Text
import Core.System
import Core.Render
import Core.Program.Arguments (Config, Parameters, parseCommandLine)

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

handleCommandLine :: Config -> IO Parameters
handleCommandLine config = do
    argv <- getArgs
    let result = parseCommandLine config argv
    case result of
        Left err -> throwString err
        Right parameters -> return parameters

