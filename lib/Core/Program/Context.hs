{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Program.Context
    ( 
        Context(..)
      , Message(..)
      , Nature(..)
    ) where

import Chrono.TimeStamp (TimeStamp)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TChan (TChan)
import System.Exit (ExitCode)

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

