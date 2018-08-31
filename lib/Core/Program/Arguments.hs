{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Program.Arguments
    ( 
        Config
      , minimalConfig
      , Parameters
      , parseCommandLine
    ) where

import Data.String
import Data.String.Here

import Core.Text
import Core.System
import Core.Render

{-
    see Context for details of naming convention
-}

data Config =
    Config [Option] [Argument]

type ShortName = Char

newtype LongName = LongName String
    deriving (Show, IsString)

data Option = Option LongName (Maybe ShortName) Text

data Argument = Argument LongName

newtype ParameterValue = ParameterValue String -- ugh
    deriving Show

type Parameters = [(LongName, ParameterValue)]

minimalConfig :: Config
minimalConfig = 
    Config [
        Option "verbose" (Just 'v') [here|
            Turn on verbose logging. 
        |]
        -- TODO "help"
    ]
    []


example :: Config
example =
    Config [
        Option "verbose" (Just 'v') [here|
            Turn on verbose logging. 
        |]
      , Option "crazy-mode-active" Nothing [here|
            Do the crazy thing, once and for all.
        |]
    ]
    [
        Argument "input-file" 
    ]
{-
        Environment "CRAZY_MODE" -- ?!?
-}


--parseCommandLine :: CommandLine -> [String] -> [(LongName, ParameterValue)]
parseCommandLine :: Config -> [String] -> Parameters
parseCommandLine config raw = [(LongName "verbose", ParameterValue "2")]
