{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- | Invoking a command-line program (be it tool or daemon) consists of
-- listing the name of its binary, optionally supplying various
-- options to adjust the behaviour of the program, and then supplying
-- mandatory arguments, if any are specified.
--
-- On startup, we parse any arguments passed in from the shell into
-- @name,value@ pairs and incorporated into the resultant configuration stored
-- in the program's Context.
--
-- Additionally, this module allows you to specify environment variables that,
-- if present, will be incorporated into the stored configuration.
--
module Core.Program.Arguments
    ( 
        Config
      , baselineConfig
      , Parameters
      , ParameterValue(..)
      , simple
      , complex
      , Options(..)
      , Commands(..)
      , LongName(..)
      , ShortName
      , parseCommandLine
    ) where

import Data.String
import Data.String.Here

import Core.Text
import Core.System
import Core.Render

type ShortName = Char

newtype LongName = LongName String
    deriving (Show, IsString)

data Config
    = Simple [Options]
    | Complex [Commands]


simple :: [Options] -> Config
simple options = Simple options

complex :: [Commands] -> Config
complex commands = Complex commands

data Commands 
    = Global [Options]
    | Command LongName Text [Options]
    | Environment [Variables]

data Options
    = Option LongName (Maybe ShortName) Text
    | Argument LongName

data Variables
    = Variable LongName Text


newtype ParameterValue = ParameterValue String -- ugh
    deriving Show

--
-- Result of having processed the command line and the environment.
--
data Parameters
    = Parameters {
          commandNameFrom :: Maybe LongName
        , parameterValuesFrom :: [(LongName, ParameterValue)]
        , environmentValuesFrom :: [(LongName, ParameterValue)]
    }

baselineConfig :: Config
baselineConfig =
    simple [
        Option "verbose" (Just 'v') [here|
            Turn on event level logging to console.
        |]
      , Option "help" (Just 'h') ""
    ]

parseCommandLine :: Config -> [String] -> Parameters
parseCommandLine config raw = Parameters Nothing [(LongName "verbose", ParameterValue "2")] []
