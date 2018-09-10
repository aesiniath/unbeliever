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
      , InvalidCommandLine(..)
    ) where

import Control.Exception.Safe (Exception(displayException))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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


data ParameterValue
    = Value String
    | Empty
    deriving (Show, Eq)

instance IsString ParameterValue where
    fromString x = Value x

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

data InvalidCommandLine
    = InvalidOption String
    | UnknownOption String
    | MissingArgument LongName
    | UnexpectedArguments [String]
    deriving Show

instance Exception InvalidCommandLine where
    displayException e = case e of
        InvalidOption arg ->
          let
            one = "Option '" ++ arg ++ "' illegal.\n\n"
            two = [here|
Options must either be long form with a double dash, for example:

    --verbose

or, when available with a short version, a single dash and a single
character. They need to be listed individually:

    -v -a

When an option takes a value it has to be in long form and the value
indicated with an equals sign, for example:

    --tempdir=/tmp

with complex values escaped according to the rules of your shell:

    --username="Ada Lovelace"

For options valid in this program, please see --help.
        |]
          in
            one ++ two
        UnknownOption name -> "Sorry, option '" ++ name ++ "' not recognized."
        MissingArgument (LongName name) -> "Mandatory argument '" ++ name ++ "' missing."
        UnexpectedArguments args ->
          let
            quoted = List.intercalate "', '" args
          in [iTrim|
Unexpected trailing arguments:

    '${quoted}'.

For arguments expected by this program, please see --help.
|]

trim :: Int -> String -> String
trim count input = unlines . map (drop count) . lines $ input

--
-- | Given a program configuration schema and the command line 
-- arguments, process them into Parameters pairs.
--
-- This throws 'UnknownOption' exception if one of the passed in options is
-- unrecognized (because at that point, we want to rabbit right back to the
-- top and bail out; there's no recovering).
--
parseCommandLine :: Config -> [String] -> Either InvalidCommandLine Parameters
parseCommandLine config argv = case config of
    Simple options ->
      let
        valids = extractValidNames options
        shorts = extractShortNames options
        needed = extractRequiredArguments options
        list1 = parsePossibleOptions valids shorts possibles
        list2 = parseRequiredArguments needed arguments
        result = (++) <$> list1 <*> list2
      in
        case result of
            Left err -> Left err
            Right params -> Right (Parameters Nothing params [])

    Complex commands ->
        Left (error "FIXME not implemented") -- FIXME
--      Parameters (Just undefined) undefined []
  where
    (possibles,arguments) = List.partition isOption argv

    isOption :: String -> Bool
    isOption arg = case arg of
        ('-':_) -> True
        _ -> False


parsePossibleOptions
    :: HashSet LongName
    -> HashMap ShortName LongName
    -> [String]
    -> Either InvalidCommandLine [(LongName,ParameterValue)]
parsePossibleOptions valids shorts args = mapM f args
  where
    f arg = case arg of
        ('-':'-':name) -> considerLongOption name
        ('-':c:[]) -> considerShortOption c
        _ -> Left (InvalidOption arg)

    considerLongOption :: String -> Either InvalidCommandLine (LongName,ParameterValue)
    considerLongOption arg =
      let
        (name,value) = List.span (/= '=') arg 
        candidate = LongName name
        -- lose the '='
        value' = case List.uncons value of
            Just (_,remainder) -> Value remainder
            Nothing -> Empty
      in
        if HashSet.member candidate valids
            then Right (candidate,value')
            else Left (UnknownOption ("--" ++ name))

    considerShortOption :: Char -> Either InvalidCommandLine (LongName,ParameterValue)
    considerShortOption c =
        case HashMap.lookup c shorts of
            Just name -> Right (name,Empty)
            Nothing -> Left (UnknownOption ['-',c])

parseRequiredArguments
    :: [LongName]
    -> [String]
    -> Either InvalidCommandLine [(LongName,ParameterValue)]
parseRequiredArguments needed args = iter needed args
  where
    iter :: [LongName] -> [String] -> Either InvalidCommandLine [(LongName,ParameterValue)]

    iter [] [] = Right []
    -- more arguments supplied than expected
    iter [] args = Left (UnexpectedArguments args)
    -- more arguments required, not satisfied
    iter (name:_) [] = Left (MissingArgument name)
    iter (name:names) (arg:args) =
        let
            deeper = iter names args
        in case deeper of
            Left e -> Left e
            Right list -> Right ((name,Value arg):list)

extractValidNames :: [Options] -> HashSet LongName
extractValidNames options =
    foldr f HashSet.empty options
  where
    f :: Options -> HashSet LongName -> HashSet LongName
    f (Option longname _ _) valids = HashSet.insert longname valids
    f _ valids = valids

extractShortNames :: [Options] -> HashMap ShortName LongName
extractShortNames options =
    foldr f HashMap.empty options
  where
    f :: Options -> HashMap ShortName LongName -> HashMap ShortName LongName
    f (Option longname shortname _) shorts = case shortname of
        Just shortchar -> HashMap.insert shortchar longname shorts
        Nothing -> shorts
    f _ shorts = shorts

extractRequiredArguments :: [Options] -> [LongName]
extractRequiredArguments arguments =
    foldr f [] arguments
  where
    f :: Options -> [LongName] -> [LongName]
    f (Argument longname _) needed = longname:needed
    f _ needed = needed

