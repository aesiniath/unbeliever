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
      , renderUsage
    ) where

import Control.Exception.Safe (Exception(displayException))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), nest
    , emptyDoc, hardline, fillBreak, align, (<+>), fillSep, indent
    , layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Util (reflow)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Data.String
import Data.String.Here
import System.Environment (getProgName)

import Core.Text
import Core.System
import Core.Render

type ShortName = Char

type Description = Text

newtype LongName = LongName String
    deriving (Show, IsString, Eq, Hashable)

instance Pretty LongName where
    pretty (LongName name) = pretty name

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
    | UnknownCommand String
    | NoCommandFound
    | HelpRequest
    deriving (Show, Eq)

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
        UnknownCommand first -> "Hm. Command '" ++ first ++ "' not recognized."
        NoCommandFound -> [iTrim|
No command specified.
Usage is of the form:

    ${programName} [GLOBAL OPTIONS] COMMAND [LOCAL OPTIONS] [ARGUMENTS]

See --help for details.
|]
        HelpRequest -> "Usage:"

programName :: String
programName = unsafePerformIO getProgName

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
    Simple options -> do
        params <- extractor options argv
        return (Parameters Nothing params [])

    Complex commands ->
      let
        globalOptions = extractGlobalOptions commands
        modes = extractValidModes commands
      in do
        (possibles,first,remainingArgs) <- splitCommandLine argv
        params1 <- extractor globalOptions possibles
        (mode,localOptions) <- parseIndicatedCommand modes first
        params2 <- extractor localOptions remainingArgs
        return (Parameters (Just mode) (params1 ++ params2) [])
  where

    extractor :: [Options] -> [String] -> Either InvalidCommandLine [(LongName,ParameterValue)]
    extractor options args =
      let
        (possibles,arguments) = List.partition isOption args
        valids = extractValidNames options
        shorts = extractShortNames options
        needed = extractRequiredArguments options
      in do
        list1 <- parsePossibleOptions valids shorts possibles
        list2 <- parseRequiredArguments needed arguments
        return (list1 ++ list2)

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
        "--help" -> Left HelpRequest
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

parseIndicatedCommand
    :: HashMap LongName [Options]
    -> String
    -> Either InvalidCommandLine (LongName,[Options])
parseIndicatedCommand modes first =
  let
    candidate = LongName first
  in
    case HashMap.lookup candidate modes of
        Just options -> Right (candidate,options)
        Nothing -> Left (UnknownCommand first)

{-
    Ok, the f,g,h,... was silly. But hey :)
-}

extractValidNames :: [Options] -> HashSet LongName
extractValidNames options =
    foldr f HashSet.empty options
  where
    f :: Options -> HashSet LongName -> HashSet LongName
    f (Option longname _ _) valids = HashSet.insert longname valids
    f _ valids = valids

extractShortNames :: [Options] -> HashMap ShortName LongName
extractShortNames options =
    foldr g HashMap.empty options
  where
    g :: Options -> HashMap ShortName LongName -> HashMap ShortName LongName
    g (Option longname shortname _) shorts = case shortname of
        Just shortchar -> HashMap.insert shortchar longname shorts
        Nothing -> shorts
    g _ shorts = shorts

extractRequiredArguments :: [Options] -> [LongName]
extractRequiredArguments arguments =
    foldr h [] arguments
  where
    h :: Options -> [LongName] -> [LongName]
    h (Argument longname _) needed = longname:needed
    h _ needed = needed

extractGlobalOptions :: [Commands] -> [Options]
extractGlobalOptions commands =
    foldr j [] commands
  where
    j :: Commands -> [Options] -> [Options]
    j (Global options) valids = options ++ valids
    j _ valids = valids

extractValidModes :: [Commands] -> HashMap LongName [Options]
extractValidModes commands =
    foldr k HashMap.empty commands
  where
    k :: Commands -> HashMap LongName [Options] -> HashMap LongName [Options]
    k (Command longname _ options) modes = HashMap.insert longname options modes
    k _ modes = modes

splitCommandLine :: [String] -> Either InvalidCommandLine ([String], String, [String])
splitCommandLine args =
  let
    (possibles,remainder) = List.span isOption args
    x = List.uncons remainder
  in
    case x of
        Just (mode,remainingArgs) -> Right (possibles,mode,remainingArgs)
        Nothing -> Left NoCommandFound


{-
    There's TODO and then there's TODO. This is the latter.
    Replace with prettyprinter doing proper formatting.
-}
renderUsage :: Config -> String
renderUsage config = case config of
    Simple options ->
      let
        (o,a) = partitionParameters options

        usage = "Usage:" <> hardline <> hardline
            <> indent 4 (nest 4 (pretty programName <> optionsSummary o <> argumentsSummary a)) <> hardline
            <> optionsHeading o
            <> formatParameters o
            <> argumentsHeading a
            <> formatParameters a
      in
        renderString (layoutPretty defaultLayoutOptions usage)

  where
    partitionParameters :: [Options] -> ([Options],[Options])
    partitionParameters options = foldr f ([],[]) options

    optionsSummary :: [Options] -> Doc ann
    optionsSummary os = if length os > 0 then " [OPTIONS]" else emptyDoc

    optionsHeading os = if length os > 0 then hardline <> "Available options:" <> hardline else emptyDoc

    argumentsSummary :: [Options] -> Doc ann
    argumentsSummary as = " " <> fillSep (fmap pretty (extractRequiredArguments as))

    argumentsHeading as = if length as > 0 then hardline <> "Required arguments:" <> hardline else emptyDoc

    f :: Options -> ([Options],[Options]) -> ([Options],[Options])
    f o@(Option _ _ _) (opts,args) = (o:opts,args)
    f a@(Argument _ _) (opts,args) = (opts,a:args)

    formatParameters :: [Options] -> Doc ann
    formatParameters [] = emptyDoc
    formatParameters options = hardline <> foldr g emptyDoc options

{-
    15 characters width for short option, long option, and two spaces. If the
    long option's name is wider than this the description will be moved to
    the next line.

    Arguments are aligned to the character of the short option; looks
    pretty good and better than waiting until column 8.
-}
    g :: Options -> Doc ann -> Doc ann
    g (Option longname shortname description) acc =
      let
        s = case shortname of
                Just shortchar -> " -" <> pretty shortchar <> ", --"
                Nothing -> "     --"
        l = pretty longname
        d = fromText description
      in
        fillBreak 15 (s <> l <> " ") <+> align (reflow d) <> hardline <> acc
    g (Argument longname description) acc =
      let
        l = pretty longname
        d = fromText description
      in
        fillBreak 15 ("  " <> l <> " ") <+> align (reflow d) <> hardline <> acc

