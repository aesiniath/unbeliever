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

newtype ParameterName = ParameterName String
    deriving (Show, IsString)

data Option = Option ParameterName (Maybe ShortName) Text

data Argument = Argument ParameterName

newtype ParameterValue = ParameterValue String -- ugh
    deriving Show

type Parameters = [(ParameterName, ParameterValue)]

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


--parseCommandLine :: CommandLine -> [String] -> [(ParameterName, ParameterValue)]
parseCommandLine :: Config -> [String] -> Parameters
parseCommandLine config raw = [(ParameterName "verbose", ParameterValue "2")]
