{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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
      , Parameters(..)
      , ParameterValue(..)
      , simple
      , complex
      , LongName(..)
      , ShortName
      , Options(..)
      , Commands(..)
      , Variables(..)
      , Description
      , parseCommandLine
    ) where

import Control.Exception.Safe (impureThrow)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.String
import Data.String.Here

import Core.Text
import Core.System
import Core.Render

type ShortName = Char

type Description = Text

newtype LongName = LongName String
    deriving (Show, IsString, Eq, Hashable)

data Config
    = Simple [Options]
    | Complex [Commands]


simple :: [Options] -> Config
simple options = Simple options

complex :: [Commands] -> Config
complex commands = Complex commands

data Commands 
    = Global [Options]
    | Command LongName Description [Options]
    | Environment [Variables]

data Options
    = Option LongName (Maybe ShortName) Description
    | Argument LongName Description

data Variables
    = Variable LongName Description


newtype ParameterValue = ParameterValue String -- ugh
    deriving (Show, IsString, Eq)

--
-- Result of having processed the command line and the environment.
--
data Parameters
    = Parameters {
          commandNameFrom :: Maybe LongName
        , parameterValuesFrom :: [(LongName, ParameterValue)]
        , environmentValuesFrom :: [(LongName, ParameterValue)]
    } deriving (Show, Eq)

baselineConfig :: Config
baselineConfig =
    simple [
        Option "verbose" (Just 'v') [here|
            Turn on event level logging to console.
        |]
      , Option "help" (Just 'h') ""
    ]

data UnknownOption = UnknownOption String deriving Show

instance Exception UnknownOption

--
-- | Given a program configuration schema and the command line 
-- arguments, process them into Parameters pairs.
--
-- This throws 'UnknownOption' exception if one of the passed in options is
-- unrecognized (because at that point, we want to rabbit right back to the
-- top and bail out; there's no recovering).
--
parseCommandLine :: Config -> [String] -> Either String Parameters
parseCommandLine config argv = case config of
    Simple options ->
      let
        valids = extractValidNames options
        result = parsePossibleOptions valids possibles
      in
        case result of
            Left err -> Left err
            Right params -> Right (Parameters Nothing params [])

    Complex commands ->
        Left "FIXME not implemented" -- FIXME
--      Parameters (Just undefined) undefined []
  where
    (possibles,arguments) = List.partition isOption argv

    isOption :: String -> Bool
    isOption arg = case arg of
        ('-':'-':name) -> True
        ('-':c:cs) -> case cs of
            [] -> True
            _  -> error arg
        _ -> False


parsePossibleOptions
    :: HashSet LongName
    -> [String]
    -> Either String [(LongName,ParameterValue)]
parsePossibleOptions valids args = mapM f args
  where
    f arg = case arg of
        ('-':'-':name) -> considerLongOption name
        ('-':c:_) -> considerShortOption c
        _ -> Left arg

    considerLongOption :: String -> Either String (LongName,ParameterValue)
    considerLongOption arg =
      let
        (name,value) = List.span (/= '=') arg 
        candidate = LongName name
        -- lose the '='
        value' = drop 1 value
      in
        if HashSet.member candidate valids
            then Right (candidate,ParameterValue value')
            else Left arg

    considerShortOption :: Char -> Either String (LongName,ParameterValue)
    considerShortOption = error "TODO" -- FIXME


--  fold [Options] into HashSet LongName
extractValidNames :: [Options] -> HashSet LongName
extractValidNames options =
    foldr f HashSet.empty options
  where
    f :: Options -> HashSet LongName -> HashSet LongName
    f (Option longname _ _) valids = HashSet.insert longname valids

