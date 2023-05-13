{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Invoking a command-line program (be it tool or daemon) consists of listing
the name of its binary, optionally supplying various options to adjust the
behaviour of the program, and then supplying mandatory arguments, if any
are specified.

On startup, we parse any arguments passed in from the shell into
@name,value@ pairs and incorporated into the resultant configuration stored
in the program's Context.

Additionally, this module allows you to specify environment variables that,
if present, will be incorporated into the stored configuration.
-}
module Core.Program.Arguments
    ( -- * Setup
      Config
    , blankConfig
    , simpleConfig
    , simpleConfig'
    , complexConfig
    , complexConfig'
    , baselineOptions
    , Parameters (..)
    , ParameterValue (..)

      -- * Options and Arguments
    , LongName (..)
    , ShortName
    , Description
    , Options (..)

      -- * Programs with Commands
    , Commands (..)
    , appendOption

      -- * Internals
    , parseCommandLine
    , extractValidEnvironments
    , InvalidCommandLine (..)
    , buildUsage
    , buildVersion
    , emptyParameters
    ) where

import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Prettyprinter
    ( Doc
    , Pretty (..)
    , align
    , emptyDoc
    , fillBreak
    , fillCat
    , fillSep
    , hardline
    , indent
    , nest
    , softline
    , (<+>)
    )
import Prettyprinter.Util (reflow)
import System.Environment (getProgName)

import Core.Data.Structures
import Core.Program.Metadata
import Core.System.Base
import Core.Text.Rope
import Core.Text.Utilities

{- |
Single letter "short" options (omitting the "@-@" prefix, obviously).
-}
type ShortName = Char

{- |
The description of an option, command, or environment variable (for use
when rendering usage information in response to @--help@ on the
command-line).
-}
type Description = Rope

{- |
The name of an option, command, or agument (omitting the "@--@" prefix in
the case of options). This identifier will be used to generate usage text
in response to @--help@ and by you later when retreiving the values of the
supplied parameters after the program has initialized.

Turn on __@OverloadedStrings@__ when specifying configurations, obviously.
-}
newtype LongName = LongName String
    deriving (Show, IsString, Eq, Hashable, Ord)

instance Key LongName

instance Pretty LongName where
    pretty (LongName name) = pretty name

instance Textual LongName where
    intoRope (LongName str) = intoRope str
    fromRope = LongName . fromRope

{- |
The setup for parsing the command-line arguments of your program. You build
a @Config@ with 'simpleConfig' or 'complexConfig', and pass it to
'Core.Program.Context.configure'.
-}
data Config
    = Blank
    | Simple Description [Options]
    | Complex Description [Commands]

--
-- Those constructors are not exposed [and functions wrapping them are] partly
-- for documentation convenience, partly for aesthetics (after a point too many
-- constructors got a bit hard to differentiate betwen), and mostly so that if
-- configure's argument turns into a monad like RequestBuilder we have
-- somewhere to make that change.
--

{- |
A completely empty configuration, without the default debugging and logging
options. Your program won't process any command-line options or arguments,
which would be weird in most cases. Prefer 'simpleConfig'.

@since 0.2.9
-}
blankConfig :: Config
blankConfig = Blank

{- |
Declare a simple (as in normal) configuration for a program with any number
of optional parameters and mandatory arguments. For example:

@
main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' \"1.0\" 'Core.Program.Execute.None' ('simpleConfig'
        [ 'Option' "host" ('Just' \'h\') 'Empty' ['quote'|
            Specify an alternate host to connect to when performing the
            frobnication. The default is \"localhost\".
          |]
        , 'Option' "port" ('Just' \'p\') 'Empty' ['quote'|
            Specify an alternate port to connect to when frobnicating.
          |]
        , 'Option' "dry-run" 'Nothing' ('Value' \"TIME\") ['quote'|
            Perform a trial run at the specified time but don't actually
            do anything.
          |]
        , 'Option' "quiet" ('Just' \'q\') 'Empty' ['quote'|
            Supress normal output.
          |]
        , 'Argument' "filename" ['quote'|
            The file you want to frobnicate.
          |]
        ])

    'Core.Program.Execute.executeWith' context program
@

which, if you build that into an executable called @snippet@ and invoke it
with @--help@, would result in:

@
\$ __./snippet --help__
Usage:

    snippet [OPTIONS] <filename>

Available options:

  -h, --host     Specify an alternate host to connect to when performing the
                 frobnication. The default is \"localhost\".
  -p, --port     Specify an alternate port to connect to when frobnicating.
      --dry-run=<TIME>
                 Perform a trial run at the specified time but don't
                 actually do anything.
  -q, --quiet    Supress normal output.
  -v, --verbose  Turn on informational messages. The logging stream will go
                 to standard output on your terminal.
      --debug    Turn on debug level logging. Implies --verbose.

Required arguments:

  <filename>     The file you want to frobnicate.
\$ __|__
@

For information on how to use the multi-line string literals shown here,
see 'quote' in "Core.Text.Utilities".

@since 0.2.9
-}
simpleConfig :: [Options] -> Config
simpleConfig options = Simple emptyRope (options ++ baselineOptions)

{- |
Declare a simple configuration as with 'simpleConfig', along with a
descriptive precis that will be printed at the top of the @--help@ output.


@
\$ __./snippet --help__
A small but very useful program.

Usage:

    snippet [OPTIONS] <filename>
...
@

@since 0.6.5
-}
simpleConfig' :: Description -> [Options] -> Config
simpleConfig' description options = Simple description (options ++ baselineOptions)

{- |
Declare a complex configuration (implying a larger tool with various
"[sub]commands" or "modes"} for a program. You can specify global options
applicable to all commands, a list of commands, and environment variables
that will be honoured by the program. Each command can have a list of local
options and arguments as needed. For example:

@
program :: 'Core.Program.Execute.Program' MusicAppStatus ()
program = ...

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' ('Core.Program.Execute.fromPackage' version) 'mempty' ('complexConfig'
        [ 'Global'
            [ 'Option' "station-name" 'Nothing' ('Value' \"NAME\") ['quote'|
                Specify an alternate radio station to connect to when performing
                actions. The default is \"BBC Radio 1\".
              |]
            , 'Variable' \"PLAYER_FORCE_HEADPHONES\" ['quote'|
                If set to @1@, override the audio subsystem to force output
                to go to the user's headphone jack.
              |]
            ]
        , 'Command' \"play\" \"Play the music.\"
            [ 'Option' "repeat" 'Nothing' 'Empty' ['quote'|
                Request that they play the same song over and over and over
                again, simulating the effect of listening to a Top 40 radio
                station.
              |]
            ]
        , 'Command' \"rate\" \"Vote on whether you like the song or not.\"
            [ 'Option' "academic" 'Nothing' 'Empty' ['quote'|
                The rating you wish to apply, from A+ to F. This is the
                default, so there is no reason whatsoever to specify this.
                But some people are obsessive, compulsive, and have time on
                their hands.
              |]
            , 'Option' "numeric" 'Nothing' 'Empty' ['quote'|
                Specify a score as a number from 0 to 100 instead of an
                academic style letter grade. Note that negative values are
                not valid scores, despite how vicerally satisfying that
                would be for music produced in the 1970s.
              |]
            , 'Option' "unicode" ('Just' \'c\') 'Empty' ['quote'|
                Instead of a score, indicate your rating with a single
                character.  This allows you to use emoji, so that you can
                rate a piece \'💩\', as so many songs deserve.
              |]
            , 'Argument' "score" ['quote'|
                The rating you wish to apply.
              |]
            ]
        ])

    'Core.Program.Execute.executeWith' context program
@

is a program with one global option (in addition to the default ones) [and
an environment variable] and two commands: @play@, with one option; and
@rate@, with two options and a required argument. It also is set up to
carry its top-level application state around in a type called
@MusicAppStatus@ (implementing 'Monoid' and so initialized here with
'mempty'. This is a good pattern to use given we are so early in the
program's lifetime).

The resultant program could be invoked as in these examples:

@
\$ __./player --station-name=\"KBBL-FM 102.5\" play__
\$
@

@
\$ __./player -v rate --numeric 76__
\$
@

For information on how to use the multi-line string literals shown here,
see 'quote' in "Core.Text.Utilities".

@since 0.2.9
-}
complexConfig :: [Commands] -> Config
complexConfig commands = Complex emptyRope (commands ++ [Global baselineOptions])

{- |
Declare a complex configuration as with 'complexConfig', along with a
descriptive precis that will be printed at the top of the @--help@ output when
requesting help for the program as a whole.

If help is requested for one of the sub commands, the description from the
'Command' constructor will be used at the top of the output.

@since 0.6.5
-}
complexConfig' :: Description -> [Commands] -> Config
complexConfig' precis commands = Complex precis (commands ++ [Global baselineOptions])

{- |
Description of the command-line structure of a program which has
\"commands\" (sometimes referred to as \"subcommands\") representing
different modes of operation. This is familiar from tools like /git/
and /docker/.
-}
data Commands
    = Global [Options]
    | Command LongName Description [Options]

{- |
Declaration of an optional switch or mandatory argument expected by a
program.

'Option' takes a long name for the option, a short single character
abbreviation if offered for convenience, whether or not the option takes a
value (and what label to show in help output) and a description for use
when displaying usage via @--help@.

'Argument' indicates a mandatory argument and takes the long name used
to identify the parsed value from the command-line, and likewise a
description for @--help@ output.

By convention option and argument names are both /lower case/. If the
identifier is two or more words they are joined with a hyphen. Examples:

@
        [ 'Option' \"quiet\" ('Just' \'q'\) 'Empty' \"Keep the noise to a minimum.\"
        , 'Option' \"dry-run\" 'Nothing' ('Value' \"TIME\") \"Run a simulation of what would happen at the specified time.\"
        , 'Argument' \"username\" \"The user to delete from the system.\"
        ]
@

By convention a /description/ is one or more complete sentences each of
which ends with a full stop. For options that take values, use /upper case/
when specifying the label to be used in help output.

'Remaining' is special; it indicates that you are expecting a variable number
of additional, non-mandatory arguments. This is used for programs which take a
list of files to process, for example. It'll show up in the help with the
description you supply alongside.

@
        [ ...
        , 'Remaining' \"The files you wish to delete permanently.\"
        , ...
        ]
@

'Variable' declares an /environment variable/ that, if present, will be
read by the program and stored in its runtime context. By convention these
are /upper case/. If the identifier is two or more words they are joined
with an underscore:

@
        [ ...
        , 'Variable' \"CRAZY_MODE\" \"Specify how many crazies to activate.\"
        , ...
        ]
@

Finally, there is a 'Descriptive' constructor which allows you to put a piece
of header text as a descriptive summary in help output before it starts
enumerating the options and arguments.

@
        [ 'Descriptive' \"A program to evaluate just how crazy you are.\"
        , ...
        ]
@
-}
data Options
    = Option LongName (Maybe ShortName) ParameterValue Description
    | Argument LongName Description
    | Remaining Description
    | Variable LongName Description
    deriving (Show)

appendOption :: Options -> Config -> Config
appendOption option config =
    case config of
        Blank -> Blank
        Simple precis options -> Simple precis (options ++ [option])
        Complex precis commands -> Complex precis (commands ++ [Global [option]])

{- |
Individual parameters read in off the command-line can either have a value
(in the case of arguments and options taking a value) or be empty (in the
case of options that are just flags).
-}
data ParameterValue
    = Value String
    | Empty
    deriving (Show, Eq)

instance IsString ParameterValue where
    fromString x = Value x

{- |
Result of having processed the command-line and the environment. You get at
the parsed command-line options and arguments by calling
'Core.Program.Execute.getCommandLine' within a
'Core.Program.Execute.Program' block.

Each option and mandatory argument parsed from the command-line is either
standalone (in the case of switches and flags, such as @--quiet@) or has an
associated value. In the case of options the key is the name of the option,
and for arguments it is the implicit name specified when setting up the
program. For example, in:

@
\$ ./submit --username=gbmh GraceHopper_Resume.pdf
@

the option has parameter name \"@username@\" and value \"@gmbh@\"; the
argument has parameter name \"filename\" (assuming that is what was
declared in the 'Argument' entry) and a value being the Admiral's CV. This
would be returned as:

@
'Parameters' 'Nothing' [("username","gbmh"), ("filename","GraceHopper_Resume.pdf")] []
@

The case of a complex command such as /git/ or /stack/, you get the specific
mode chosen by the user returned in the first position:

@
\$ missiles launch --all
@

would be parsed as:

@
'Parameters' ('Just' \"launch\") [("all",Empty)] []
@
-}
data Parameters = Parameters
    { commandNameFrom :: Maybe LongName
    , parameterValuesFrom :: Map LongName ParameterValue
    , remainingArgumentsFrom :: [String]
    , environmentValuesFrom :: Map LongName ParameterValue
    }
    deriving (Show, Eq)

emptyParameters :: Parameters
emptyParameters =
    Parameters
        { commandNameFrom = Nothing
        , parameterValuesFrom = emptyMap
        , remainingArgumentsFrom = []
        , environmentValuesFrom = emptyMap
        }

baselineOptions :: [Options]
baselineOptions =
    [ Option
        "verbose"
        (Just 'v')
        Empty
        [quote|
        Turn on informational messages. The logging stream will go
        to standard output in your terminal.
    |]
    , Option
        "debug"
        Nothing
        Empty
        [quote|
        Turn on debug output. Implies --verbose.
    |]
    ]

{- |
Different ways parsing a simple or complex command-line can fail.
-}
data InvalidCommandLine
    = -- | Something was wrong with the way the user specified [usually a short] option.
      InvalidOption String
    | -- | User specified an option that doesn't match any in the supplied configuration.
      UnknownOption String
    | -- | Arguments are mandatory, and this one is missing.
      MissingArgument LongName
    | -- | Arguments are present we weren't expecting.
      UnexpectedArguments [String]
    | -- | In a complex configuration, user specified a command that doesn't match any in the configuration.
      UnknownCommand String
    | -- | In a complex configuration, user didn't specify a command.
      NoCommandFound
    | -- | In a complex configuration, usage information was requested with @--help@, either globally or for the supplied command.
      HelpRequest (Maybe LongName)
    | -- | Display of the program version requested with @--version@.
      VersionRequest
    deriving (Show, Eq)

instance Exception InvalidCommandLine where
    displayException e = case e of
        InvalidOption arg ->
            let one = "Option '" ++ arg ++ "' illegal.\n\n"
                two =
                    [quote|
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
            in  one ++ two
        UnknownOption name -> "Sorry, option '" ++ name ++ "' not recognized."
        MissingArgument (LongName name) -> "Mandatory argument '" ++ name ++ "' missing."
        UnexpectedArguments args ->
            let quoted = List.intercalate "', '" args
            in  [quote|
Unexpected trailing arguments:

|]
                    ++ quoted
                    ++ [quote|

For arguments expected by this program, please see --help.
|]
        UnknownCommand first -> "Hm. Command '" ++ first ++ "' not recognized."
        NoCommandFound ->
            [quote|
No command specified.
Usage is of the form:

    |]
                ++ programName
                ++ [quote| [GLOBAL OPTIONS] COMMAND [LOCAL OPTIONS] [ARGUMENTS]

See --help for details.
|]
        -- handled by parent module calling back into here buildUsage
        HelpRequest _ -> ""
        -- handled by parent module calling back into here buildVersion
        VersionRequest -> ""

programName :: String
programName = unsafePerformIO getProgName

{- |
Given a program configuration schema and the command-line arguments, process
them into key/value pairs in a Parameters object.

This results in 'InvalidCommandLine' on the left side if one of the passed in
options is unrecognized or if there is some other problem handling options or
arguments (because at that point, we want to rabbit right back to the top and
bail out; there's no recovering).

This isn't something you'll ever need to call directly; it's exposed for
testing convenience. This function is invoked when you call
'Core.Program.Context.configure' or 'Core.Program.Execute.execute' (which
calls 'Core.Program.Context.configure' with a default 'Config' when
initializing).
-}
parseCommandLine :: Config -> [String] -> Either InvalidCommandLine Parameters
parseCommandLine config argv = case config of
    Blank -> return (Parameters Nothing emptyMap [] emptyMap)
    Simple _ options -> do
        (params, remainder) <- extractor Nothing options argv
        checkRemainder options remainder
        return (Parameters Nothing params remainder emptyMap)
    Complex _ commands ->
        let globalOptions = extractGlobalOptions commands
            modes = extractValidModes commands
        in  do
                (possibles, argv') <- splitCommandLine1 argv
                (params1, _) <- extractor Nothing globalOptions possibles
                (first, moreArgs) <- splitCommandLine2 argv'
                (mode, localOptions) <- parseIndicatedCommand modes first
                (params2, remainder) <- extractor (Just mode) localOptions moreArgs
                checkRemainder localOptions remainder
                return (Parameters (Just mode) ((<>) params1 params2) remainder emptyMap)
  where
    extractor :: Maybe LongName -> [Options] -> [String] -> Either InvalidCommandLine ((Map LongName ParameterValue), [String])
    extractor mode options args =
        let (possibles, arguments) = List.partition isOption args
            valids = extractValidNames options
            shorts = extractShortNames options
            needed = extractRequiredArguments options
        in  do
                list1 <- parsePossibleOptions mode valids shorts possibles
                (list2, arguments') <- parseRequiredArguments needed arguments
                pure (((<>) (intoMap list1) (intoMap list2)), arguments')

    checkRemainder :: [Options] -> [String] -> Either InvalidCommandLine ()
    checkRemainder options remainder =
        if List.null remainder
            then Right ()
            else
                if hasRemaining options
                    then Right ()
                    else Left (UnexpectedArguments remainder)

-- is one of the options Remaining?
hasRemaining :: [Options] -> Bool
hasRemaining options =
    List.foldl'
        ( \acc option -> case option of
            Remaining _ -> True
            _ -> acc
        )
        False
        options

isOption :: String -> Bool
isOption arg = case arg of
    ('-' : _) -> True
    _ -> False

parsePossibleOptions
    :: Maybe LongName
    -> Set LongName
    -> Map ShortName LongName
    -> [String]
    -> Either InvalidCommandLine [(LongName, ParameterValue)]
parsePossibleOptions mode valids shorts args = mapM f args
  where
    f arg = case arg of
        "--help" -> Left (HelpRequest mode)
        "-?" -> Left (HelpRequest mode)
        "--version" -> Left VersionRequest
        ('-' : '-' : name) -> considerLongOption name
        ('-' : c : []) -> considerShortOption c
        _ -> Left (InvalidOption arg)

    considerLongOption :: String -> Either InvalidCommandLine (LongName, ParameterValue)
    considerLongOption arg =
        let (name, value) = List.span (/= '=') arg
            candidate = LongName name
            -- lose the '='
            value' = case List.uncons value of
                Just (_, remainder) -> Value remainder
                Nothing -> Empty
        in  if containsElement candidate valids
                then Right (candidate, value')
                else Left (UnknownOption ("--" ++ name))

    considerShortOption :: Char -> Either InvalidCommandLine (LongName, ParameterValue)
    considerShortOption c =
        case lookupKeyValue c shorts of
            Just name -> Right (name, Empty)
            Nothing -> Left (UnknownOption ['-', c])

parseRequiredArguments
    :: [LongName]
    -> [String]
    -> Either InvalidCommandLine ([(LongName, ParameterValue)], [String])
parseRequiredArguments needed argv = iter needed argv
  where
    iter :: [LongName] -> [String] -> Either InvalidCommandLine ([(LongName, ParameterValue)], [String])
    iter [] [] = Right ([], [])
    -- more arguments supplied than expected
    iter [] args = Right ([], args)
    -- more arguments required, not satisfied
    iter (name : _) [] = Left (MissingArgument name)
    iter (name : names) (arg : args) =
        let deeper = iter names args
        in  case deeper of
                Left e -> Left e
                Right (list, remainder) -> Right (((name, Value arg) : list), remainder)

parseIndicatedCommand
    :: Map LongName (Description, [Options])
    -> String
    -> Either InvalidCommandLine (LongName, [Options])
parseIndicatedCommand modes first =
    let candidate = LongName first
    in  case lookupKeyValue candidate modes of
            Just (_, options) -> Right (candidate, options)
            Nothing -> Left (UnknownCommand first)

--
-- Ok, the f,g,h,... was silly. But hey :)
--

extractValidNames :: [Options] -> Set LongName
extractValidNames options =
    foldr f emptySet options
  where
    f :: Options -> Set LongName -> Set LongName
    f (Option longname _ _ _) valids = insertElement longname valids
    f _ valids = valids

extractShortNames :: [Options] -> Map ShortName LongName
extractShortNames options =
    foldr g emptyMap options
  where
    g :: Options -> Map ShortName LongName -> Map ShortName LongName
    g (Option longname shortname _ _) shorts = case shortname of
        Just shortchar -> insertKeyValue shortchar longname shorts
        Nothing -> shorts
    g _ shorts = shorts

extractRequiredArguments :: [Options] -> [LongName]
extractRequiredArguments arguments =
    List.foldr h [] arguments
  where
    h :: Options -> [LongName] -> [LongName]
    h (Argument longname _) needed = longname : needed
    h _ needed = needed

extractGlobalOptions :: [Commands] -> [Options]
extractGlobalOptions commands =
    foldr j [] commands
  where
    j :: Commands -> [Options] -> [Options]
    j (Global options) valids = options ++ valids
    j _ valids = valids

extractValidModes :: [Commands] -> Map LongName (Description, [Options])
extractValidModes commands =
    List.foldl' k emptyMap commands
  where
    k :: Map LongName (Description, [Options]) -> Commands -> Map LongName (Description, [Options])
    k modes (Command longname description options) = insertKeyValue longname (description, options) modes
    k modes _ = modes

{-
Break the command-line apart in two steps. The first peels off the global
options, the second below looks to see if there is a command (of fails) and
if so, whether it has any parameters.

We do it this way so that `parseCommandLine` can pas the global options to
`extractor` and thence `parsePossibleOptions` to catch --version and
--help.
-}
splitCommandLine1 :: [String] -> Either InvalidCommandLine ([String], [String])
splitCommandLine1 args =
    let (possibles, remainder) = List.span isOption args
    in  if null possibles && null remainder
            then Left NoCommandFound
            else Right (possibles, remainder)

splitCommandLine2 :: [String] -> Either InvalidCommandLine (String, [String])
splitCommandLine2 argv' =
    let x = List.uncons argv'
    in  case x of
            Just (mode, remainingArgs) -> Right (mode, remainingArgs)
            Nothing -> Left NoCommandFound

--
-- Environment variable handling
--

extractValidEnvironments :: Maybe LongName -> Config -> Set LongName
extractValidEnvironments mode config = case config of
    Blank -> emptySet
    Simple _ options -> extractVariableNames options
    Complex _ commands ->
        let globals = extractGlobalOptions commands
            variables1 = extractVariableNames globals

            locals = extractLocalVariables commands (fromMaybe "" mode)
            variables2 = extractVariableNames locals
        in  variables1 <> variables2

extractLocalVariables :: [Commands] -> LongName -> [Options]
extractLocalVariables commands mode =
    foldr k [] commands
  where
    k :: Commands -> [Options] -> [Options]
    k (Command name _ options) acc = if name == mode then options else acc
    k _ acc = acc

extractVariableNames :: [Options] -> Set LongName
extractVariableNames options =
    foldr f emptySet options
  where
    f :: Options -> Set LongName -> Set LongName
    f (Variable longname _) valids = insertElement longname valids
    f _ valids = valids

--
-- The code from here on is formatting code. It's fairly repetative
-- and crafted to achieve a specific aesthetic output. Rather messy.
-- I'm sure it could be done "better" but no matter; this is on the
-- path to an exit and return to user's shell prompt.
--

buildUsage :: Config -> Maybe LongName -> Doc ann
buildUsage config mode = case config of
    Blank -> emptyDoc
    Simple precis options ->
        let (o, a, v) = partitionParameters options
        in  formatPrecis precis
                <> "Usage:"
                <> hardline
                <> hardline
                <> indent
                    4
                    ( nest
                        4
                        ( fillCat
                            [ pretty programName
                            , optionsSummary o
                            , argumentsSummary a
                            , remainingSummary a
                            ]
                        )
                    )
                <> hardline
                <> optionsHeading o
                <> formatParameters o
                <> argumentsHeading a
                <> formatParameters a
                <> variablesHeading v
                <> formatParameters v
    Complex precis commands ->
        let globalOptions = extractGlobalOptions commands
            modes = extractValidModes commands

            (oG, _, vG) = partitionParameters globalOptions
        in  case mode of
                Nothing ->
                    formatPrecis precis
                        <> "Usage:"
                        <> hardline
                        <> hardline
                        <> indent
                            2
                            ( nest
                                4
                                ( fillCat
                                    [ pretty programName
                                    , globalSummary oG
                                    , commandSummary modes
                                    ]
                                )
                            )
                        <> hardline
                        <> globalHeading oG
                        <> formatParameters oG
                        <> commandHeading modes
                        <> formatCommands commands
                        <> variablesHeading vG
                        <> formatParameters vG
                Just longname ->
                    let (dL, (oL, aL, vL)) = case lookupKeyValue longname modes of
                            Just (description, localOptions) -> (description, partitionParameters localOptions)
                            Nothing -> error "Illegal State"
                    in  formatPrecis dL
                            <> "Usage:"
                            <> hardline
                            <> hardline
                            <> indent
                                2
                                ( nest
                                    4
                                    ( fillCat
                                        [ pretty programName
                                        , globalSummary oG
                                        , commandSummary modes
                                        , localSummary oL
                                        , argumentsSummary aL
                                        , remainingSummary aL
                                        ]
                                    )
                                )
                            <> hardline
                            <> localHeading oL
                            <> formatParameters oL
                            <> argumentsHeading aL
                            <> formatParameters aL
                            <> variablesHeading vL
                            <> formatParameters vL
  where
    formatPrecis :: Description -> Doc ann
    formatPrecis precis = case widthRope precis of
        0 -> emptyDoc
        _ -> reflow (fromRope precis) <> hardline <> hardline

    partitionParameters :: [Options] -> ([Options], [Options], [Options])
    partitionParameters options = List.foldl' f ([], [], []) options

    optionsSummary :: [Options] -> Doc ann
    optionsSummary os = if length os > 0 then softline <> "[OPTIONS]" else emptyDoc

    optionsHeading os = if length os > 0 then hardline <> "Available options:" <> hardline else emptyDoc

    globalSummary os = if length os > 0 then softline <> "[GLOBAL OPTIONS]" else emptyDoc
    globalHeading os =
        if length os > 0
            then hardline <> "Global options:" <> hardline
            else emptyDoc

    localSummary os = if length os > 0 then softline <> "[LOCAL OPTIONS]" else emptyDoc
    localHeading os =
        if length os > 0
            then hardline <> "Options to the '" <> commandName <> "' command:" <> hardline
            else emptyDoc

    commandName :: Doc ann
    commandName = case mode of
        Just (LongName name) -> pretty name
        Nothing -> "COMMAND..."

    argumentsSummary :: [Options] -> Doc ann
    argumentsSummary as = " " <> fillSep (fmap (\x -> "<" <> pretty x <> ">") (extractRequiredArguments as))

    argumentsHeading as = if length as > 0 then hardline <> "Required arguments:" <> hardline else emptyDoc

    variablesHeading vs = if length vs > 0 then hardline <> "Known environment variables:" <> hardline else emptyDoc

    remainingSummary :: [Options] -> Doc ann
    remainingSummary as = if hasRemaining as then " ..." else emptyDoc

    -- there is a corner case of complex config with no commands
    commandSummary modes = if length modes > 0 then softline <> commandName else emptyDoc
    commandHeading modes = if length modes > 0 then hardline <> "Available commands:" <> hardline else emptyDoc

    f :: ([Options], [Options], [Options]) -> Options -> ([Options], [Options], [Options])
    f (opts, args, vars) o@(Option _ _ _ _) = (o : opts, args, vars)
    f (opts, args, vars) a@(Argument _ _) = (opts, a : args, vars)
    f (opts, args, vars) a@(Remaining _) = (opts, a : args, vars)
    f (opts, args, vars) v@(Variable _ _) = (opts, args, v : vars)

    formatParameters :: [Options] -> Doc ann
    formatParameters [] = emptyDoc
    formatParameters options = hardline <> List.foldl' g emptyDoc options

    --
    -- 16 characters width for short option, long option, and two spaces. If the
    -- long option's name is wider than this the description will be moved to
    -- the next line.
    --
    -- Arguments are aligned to the character of the short option; looks
    -- pretty good and better than waiting until column 8.
    --

    g :: Doc ann -> Options -> Doc ann
    g acc (Option longname shortname valued description) =
        let s = case shortname of
                Just shortchar -> "  -" <> pretty shortchar <> ", --"
                Nothing -> "      --"
            l = pretty longname
            d = fromRope description
        in  case valued of
                Empty ->
                    fillBreak 16 (s <> l <> " ") <+> align (reflow d) <> hardline <> acc
                Value label ->
                    fillBreak 16 (s <> l <> "=<" <> pretty label <> "> ") <+> align (reflow d) <> hardline <> acc
    g acc (Argument longname description) =
        let l = pretty longname
            d = fromRope description
        in  fillBreak 16 ("  <" <> l <> "> ") <+> align (reflow d) <> hardline <> acc
    g acc (Remaining description) =
        let d = fromRope description
        in  fillBreak 16 ("  " <> "... ") <+> align (reflow d) <> hardline <> acc
    g acc (Variable longname description) =
        let l = pretty longname
            d = fromRope description
        in  fillBreak 16 ("  " <> l <> " ") <+> align (reflow d) <> hardline <> acc

    formatCommands :: [Commands] -> Doc ann
    formatCommands commands = hardline <> List.foldl' h emptyDoc commands

    h :: Doc ann -> Commands -> Doc ann
    h acc (Command longname description _) =
        let l = pretty longname
            d = fromRope description
        in  acc <> fillBreak 16 ("  " <> l <> " ") <+> align (reflow d) <> hardline
    h acc _ = acc

buildVersion :: Version -> Doc ann
buildVersion version =
    let
        project = projectNameFrom version
        number = versionNumberFrom version
        description = gitDescriptionFrom version
    in
        pretty project
            <+> pretty number
                <> if null description
                    then hardline
                    else "," <+> pretty description <> hardline
