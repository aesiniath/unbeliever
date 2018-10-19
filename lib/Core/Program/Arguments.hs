{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
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
    ( 
        {-* Setup -}
        Config
      , blank
      , simple
      , complex
      , baselineOptions
      , Parameters(..)
      , ParameterValue(..)
      , lookupArgument
        {-* Options and Arguments -}
      , LongName(..)
      , ShortName
      , Description
      , Options(..)
        {-* Programs with Commands -}
      , Commands(..)
        {-* Internals -}
      , parseCommandLine
      , extractValidEnvironments
      , InvalidCommandLine(..)
      , buildUsage
    ) where

import Control.Exception.Safe (Exception(displayException))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), nest, fillCat
    , emptyDoc, hardline, softline, fillBreak, align, (<+>), fillSep, indent)
import Data.Text.Prettyprint.Doc.Util (reflow)
import Data.String
import System.Environment (getProgName)

import Core.System.Base
import Core.Text.Rope
import Core.Text.Utilities

{-|
Single letter "short" options (omitting the "@-@" prefix, obviously).
-}
type ShortName = Char

{-|
The description of an option, command, or environment variable (for use
when rendering usage information in response to @--help@ on the
command-line).
-}
type Description = Rope

{-|
The name of an option, command, or agument (omitting the "@--@" prefix in
the case of options). This identifier will be used to generate usage text
in response to @--help@ and by you later when retreiving the values of the
supplied parameters after the program has initialized.

Turn on __@OverloadedStrings@__ when specifying configurations, obviously.
-}
newtype LongName = LongName String
    deriving (Show, IsString, Eq, Hashable)

instance Pretty LongName where
    pretty (LongName name) = pretty name

{-|
The setup for parsing the command-line arguments of your program. You build
a @Config@ with 'simple' or 'complex', and pass it to
'Core.Program.Context.configure'.
-}
data Config
    = Blank
    | Simple [Options]
    | Complex [Commands]

--
-- Those constructors are not exposed [and functions wrapping them are] partly
-- for documentation convenience, partly for aesthetics (after a point too many
-- constructors got a bit hard to differentiate betwen), and mostly so that if
-- configure's argument turns into a monad like RequestBuilder we have
-- somewhere to make that change.
--

{-|
A completely empty configuration, without the default debugging and logging
options. Your program won't process any command-line options or arguments,
which would be weird in most cases. Prefer 'simple'.
-}
blank :: Config
blank = Blank

{-|
Declare a simple (as in normal) configuration for a program with any number
of optional parameters and mandatory arguments. For example:

@
main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' 'Core.Program.Execute.None' ('simple'
        [ 'Option' "host" ('Just' \'h\') ['quote'|
            Specify an alternate host to connect to when performing the
            frobnication. The default is \"localhost\".
          |]
        , 'Option' "port" ('Just' \'p\') ['quote'|
            Specify an alternate port to connect to when frobnicating.
          |]
        , 'Option' "dry-run" 'Nothing' ['quote'|
            Perform a trial run but don't actually do anything.
          |]
        , 'Option' "quiet" ('Just' \'q\') ['quote'|
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
$ __./snippet --help__
Usage:

    snippet [OPTIONS] filename

Available options:

  -h, --host     Specify an alternate host to connect to when performing the
                 frobnication. The default is \"localhost\".
  -p, --port     Specify an alternate port to connect to when frobnicating.
      --dry-run  Perform a trial run but don't actually do anything.
  -q, --quiet    Supress normal output.
  -v, --verbose  Turn on event tracing. By default the logging stream will go
                 to standard output on your terminal.
      --debug    Turn on debug level logging. Implies --verbose.
      --logging  Change where log messages are sent. Valid values are
                 \"console\", \"file:\/path\/to\/filename.log\", and \"syslog\".

Required arguments:

  filename       The file you want to frobnicate.
$ __|__
@

For information on how to use the multi-line string literals shown here,
see 'quote' in "Core.Text.Utilities".
-}
simple :: [Options] -> Config
simple options = Simple (options ++ baselineOptions)

{-|
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
    context <- 'Core.Program.Execute.configure' 'mempty' ('complex'
        [ 'Global'
            [ 'Option' "station-name" ('Just' \'s\') ['quote'|
                Specify an alternate radio station to connect to when performing
                actions. The default is \"BBC Radio 1\".
              |]
            , 'Variable' \"PLAYER_FORCE_HEADPHONES\" ['quote'|
                If set to @1@, override the audio subsystem to force output
                to go to the user's headphone jack.
              |]
            ]
        , 'Command' \"play\" \"Play the music.\"
            [ 'Option' "repeat" 'Nothing' ['quote'|
                Request that they play the same song over and over and over
                again, simulating the effect of listening to a Top 40 radio
                station.
              |]
            ]
        , 'Command' \"rate\" \"Vote on whether you like the song or not.\"
            [ 'Option' "academic" 'Nothing' ['quote'|
                The rating you wish to apply, from A+ to F. This is the
                default, so there is no reason whatsoever to specify this.
                But some people are obsessive, compulsive, and have time on
                their hands.
              |]
            , 'Option' "numeric" 'Nothing' ['quote'|
                Specify a score as a number from 0 to 100 instead of an
                academic style letter grade. Note that negative values are
                not valid scores, despite how vicerally satisfying that
                would be for music produced in the 1970s.
              |]
            , 'Option' "unicode" ('Just' \'c\') ['quote'|
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
$ __./player --station-name=\"KBBL-FM 102.5\" play__
$
@

@
$ __./player -v rate --numeric 76__
$
@

For information on how to use the multi-line string literals shown here,
see 'quote' in "Core.Text.Utilities".
-}
complex :: [Commands] -> Config
complex commands = Complex (commands ++ [Global baselineOptions])

{-|
Description of the command-line structure of a program which has
\"commands\" (sometimes referred to as \"subcommands\") representing
different modes of operation. This is familiar from tools like /git/
and /docker/.
-}
data Commands 
    = Global [Options]
    | Command LongName Description [Options]

{-|
Declaration of an optional switch or mandatory argument expected by a
program.

'Option' takes a long name for the option, a short single character
abbreviation if offered for convenience, and a description for use when
displaying usage via @--help@.

'Argument' indicates a mandatory argument and takes the long name used
to identify the parsed value from the command-line, and likewise a
description for @--help@ output.

By convention these are both /lower case/. If the identifier is two or
more words they are joined with a hyphen. Examples:

@
        [ 'Option' \"dry-run\" 'Nothing' "Don't actually execute commands, just simulate what would happen."
        , 'Option' \"quiet\" ('Just' \'q'\) "Keep the noise to a minimum."
        , 'Argument' \"username\" "The user to delete from the system."
        ]
@

By convention a /description/ is one or more complete sentences each of
which ends with a full stop.

'Variable' declares an /environment variable/ that, if present, will be
read by the program and stored in its runtime context. By convention these
are /upper case/. If the identifier is two or more words they are joined
with an underscore:

@
        [ ...
        , 'Variable' \"CRAZY_MODE\" "Specify how many crazies to activate."
        , ...
        ]
@
-}
data Options
    = Option LongName (Maybe ShortName) Description
    | Argument LongName Description
    | Variable LongName Description


{-|
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

{-|
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
$ ./submit --username=gbmh GraceHopper_Resume.pdf
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
$ missiles launch --all
@

would be parsed as:

@
'Parameters' ('Just' "missiles") [("all",Empty)] []
@

-}
data Parameters
    = Parameters {
          commandNameFrom :: Maybe LongName
        , parameterValuesFrom :: HashMap LongName ParameterValue
        , environmentValuesFrom :: HashMap LongName ParameterValue
    } deriving (Show, Eq)

{-|
Arguments are mandatory, so by the time your program is running a value
has already been identified. This returns the value for that parameter.
-}
lookupArgument :: LongName -> Parameters -> String
lookupArgument name params =
    case HashMap.lookup name (parameterValuesFrom params) of
        Nothing -> invalid
        Just argument -> case argument of
            Empty -> invalid
            Value value -> value

-- Illegal internal state resulting from programmer error
invalid :: a
invalid = error "Invalid State"

baselineOptions :: [Options]
baselineOptions =
    [ Option "verbose" (Just 'v') [quote|
        Turn on event tracing. By default the logging stream will go to
        standard output on your terminal.
    |]
    , Option "debug" Nothing [quote|
        Turn on debug level logging. Implies --verbose.
    |]
    , Option "logging" Nothing [quote|
        Change where log messages are sent. Valid values are "console",
        "file:/path/to/filename.log", and "syslog".
    |]
    ]

{-|
Different ways parsing a simple or complex command-line can fail.
-}
data InvalidCommandLine
    = InvalidOption String  {-^ Something was wrong with the way the user specified [usually a short] option. -}
    | UnknownOption String  {-^ User specified an option that doesn't match any in the supplied configuration. -}
    | MissingArgument LongName
                            {-^ Arguments are mandatory, and this one is missing. -}
    | UnexpectedArguments [String]
                            {-^ Arguments are present we weren't expecting. -}
    | UnknownCommand String {-^ In a complex configuration, user specified a command that doesn't match any in the configuration. -}
    | NoCommandFound        {-^ In a complex configuration, user didn't specify a command. -}
    | HelpRequest (Maybe LongName)
                            {-^ In a complex configuration, usage information was requested with @--help@, either globally or for the supplied command. -}
    deriving (Show, Eq)

instance Exception InvalidCommandLine where
    displayException e = case e of
        InvalidOption arg ->
          let
            one = "Option '" ++ arg ++ "' illegal.\n\n"
            two = [quote|
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
          in [quote|
Unexpected trailing arguments:

|] ++ quoted ++ [quote|

For arguments expected by this program, please see --help.
|]
        UnknownCommand first -> "Hm. Command '" ++ first ++ "' not recognized."
        NoCommandFound -> [quote|
No command specified.
Usage is of the form:

    |] ++ programName ++ [quote| [GLOBAL OPTIONS] COMMAND [LOCAL OPTIONS] [ARGUMENTS]

See --help for details.
|]
        -- handled by parent module calling back into here buildUsage
        HelpRequest _ -> ""

programName :: String
programName = unsafePerformIO getProgName

{-|
Given a program configuration schema and the command-line arguments,
process them into key/value pairs in a Parameters object.

This results in 'InvalidCommandLine' on the left side if one of the passed
in options is unrecognized or if there is some other problem handling
options or arguments (because at that point, we want to rabbit right back
to the top and bail out; there's no recovering).

This isn't something you'll ever need to call directly; it's exposed for
testing convenience. This function is invoked when you call
'Core.Program.Context.configure' or 'Core.Program.Execute.execute' (which
calls 'configure' with a default @Config@ when initializing).
-}
parseCommandLine :: Config -> [String] -> Either InvalidCommandLine Parameters
parseCommandLine config argv = case config of
    Blank -> return (Parameters Nothing HashMap.empty HashMap.empty)

    Simple options -> do
        params <- extractor Nothing options argv
        return (Parameters Nothing params HashMap.empty)

    Complex commands ->
      let
        globalOptions = extractGlobalOptions commands
        modes = extractValidModes commands
      in do
        (possibles,first,remainingArgs) <- splitCommandLine argv
        params1 <- extractor Nothing globalOptions possibles
        (mode,localOptions) <- parseIndicatedCommand modes first
        params2 <- extractor (Just mode) localOptions remainingArgs
        return (Parameters (Just mode) (HashMap.union params1 params2) HashMap.empty)
  where

    extractor :: Maybe LongName -> [Options] -> [String] -> Either InvalidCommandLine (HashMap LongName ParameterValue)
    extractor mode options args =
      let
        (possibles,arguments) = List.partition isOption args
        valids = extractValidNames options
        shorts = extractShortNames options
        needed = extractRequiredArguments options
      in do
        list1 <- parsePossibleOptions mode valids shorts possibles
        list2 <- parseRequiredArguments needed arguments
        return (HashMap.union (HashMap.fromList list1) (HashMap.fromList list2))

isOption :: String -> Bool
isOption arg = case arg of
    ('-':_) -> True
    _ -> False

parsePossibleOptions
    :: Maybe LongName
    -> HashSet LongName
    -> HashMap ShortName LongName
    -> [String]
    -> Either InvalidCommandLine [(LongName,ParameterValue)]
parsePossibleOptions mode valids shorts args = mapM f args
  where
    f arg = case arg of
        "--help" -> Left (HelpRequest mode)
        "-?"     -> Left (HelpRequest mode)
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
parseRequiredArguments needed argv = iter needed argv
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

--
-- Ok, the f,g,h,... was silly. But hey :)
--

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
        Nothing -> if (List.elem "--help" possibles)
            then Left (HelpRequest Nothing)
            else Left NoCommandFound

--
-- Environment variable handling
--

extractValidEnvironments :: Maybe LongName -> Config -> HashSet LongName
extractValidEnvironments mode config = case config of
    Blank -> HashSet.empty

    Simple options -> extractVariableNames options

    Complex commands ->
      let
        globals = extractGlobalOptions commands
        variables1 = extractVariableNames globals

        locals = extractLocalVariables commands (fromMaybe "" mode)
        variables2 = extractVariableNames locals
      in
        variables1 <> variables2

extractLocalVariables :: [Commands] -> LongName -> [Options]
extractLocalVariables commands mode =
    foldr k [] commands
  where
    k :: Commands -> [Options] -> [Options]
    k (Command name _ options) acc = if name == mode then options else acc
    k _ acc = acc


extractVariableNames :: [Options] -> HashSet LongName
extractVariableNames options =
    foldr f HashSet.empty options
  where
    f :: Options -> HashSet LongName -> HashSet LongName
    f (Variable longname _) valids = HashSet.insert longname valids
    f _ valids = valids



--
-- The code from here on is formatting code. It's fairly repetative
-- and crafted to achieve a specific aesthetic output. Rather messy.
-- I'm sure it could be done "better" but no matter; this is on the
-- path to an exit and return to user's command line.
--

buildUsage :: Config -> Maybe LongName -> Doc ann
buildUsage config mode = case config of
    Blank -> emptyDoc

    Simple options ->
      let
        (o,a) = partitionParameters options
      in
        "Usage:" <> hardline <> hardline
            <> indent 4 (nest 4 (fillCat
                [ pretty programName
                , optionsSummary o
                , argumentsSummary a
                ])) <> hardline
            <> optionsHeading o
            <> formatParameters o
            <> argumentsHeading a
            <> formatParameters a

    Complex commands ->
      let
        globalOptions = extractGlobalOptions commands
        modes = extractValidModes commands

        (oG,_) = partitionParameters globalOptions
      in
        "Usage:" <> hardline <> hardline <> case mode of
            Nothing ->
                indent 2 (nest 4 (fillCat
                    [ pretty programName
                    , globalSummary oG
                    , commandSummary modes
                    ])) <> hardline
                <> globalHeading oG
                <> formatParameters oG
                <> commandHeading modes
                <> formatCommands commands

            Just longname ->
              let
                (oL,aL) = case HashMap.lookup longname modes of
                    Just localOptions -> partitionParameters localOptions
                    Nothing -> error "Illegal State"
              in
                indent 2 (nest 4 (fillCat
                    [ pretty programName
                    , globalSummary oG
                    , commandSummary modes
                    , localSummary oL
                    , argumentsSummary aL
                    ])) <> hardline
                <> localHeading oL
                <> formatParameters oL
                <> argumentsHeading aL
                <> formatParameters aL

  where
    partitionParameters :: [Options] -> ([Options],[Options])
    partitionParameters options = foldr f ([],[]) options

    optionsSummary :: [Options] -> Doc ann
    optionsSummary os = if length os > 0 then softline <> "[OPTIONS]" else emptyDoc

    optionsHeading os = if length os > 0 then hardline <> "Available options:" <> hardline else emptyDoc

    globalSummary os = if length os > 0 then softline <> "[GLOBAL OPTIONS]" else emptyDoc
    globalHeading os = if length os > 0
        then hardline <> "Global options:" <> hardline
        else emptyDoc

    localSummary os = if length os > 0 then softline <> "[LOCAL OPTIONS]" else emptyDoc
    localHeading os = if length os > 0
        then hardline <> "Options to the '" <> commandName <> "' command:" <> hardline
        else emptyDoc

    commandName :: Doc ann
    commandName = case mode of
        Just (LongName name) -> pretty name
        Nothing -> "COMMAND..."

    argumentsSummary :: [Options] -> Doc ann
    argumentsSummary as = " " <> fillSep (fmap pretty (extractRequiredArguments as))

    argumentsHeading as = if length as > 0 then hardline <> "Required arguments:" <> hardline else emptyDoc

    -- there is a corner case of complex config with no commands
    commandSummary modes = if HashMap.size modes > 0 then softline <> commandName else emptyDoc
    commandHeading modes = if HashMap.size modes > 0 then hardline <> "Available commands:" <> hardline else emptyDoc

    f :: Options -> ([Options],[Options]) -> ([Options],[Options])
    f o@(Option _ _ _) (opts,args) = (o:opts,args)
    f a@(Argument _ _) (opts,args) = (opts,a:args)
    f (Variable _ _) (opts,args) = (opts,args)

    formatParameters :: [Options] -> Doc ann
    formatParameters [] = emptyDoc
    formatParameters options = hardline <> foldr g emptyDoc options

--
-- 16 characters width for short option, long option, and two spaces. If the
-- long option's name is wider than this the description will be moved to
-- the next line.
--
-- Arguments are aligned to the character of the short option; looks
-- pretty good and better than waiting until column 8.
--

    g :: Options -> Doc ann -> Doc ann
    g (Option longname shortname description) acc =
      let
        s = case shortname of
                Just shortchar -> "  -" <> pretty shortchar <> ", --"
                Nothing -> "      --"
        l = pretty longname
        d = fromRope description
      in
        fillBreak 16 (s <> l <> " ") <+> align (reflow d) <> hardline <> acc
    g (Argument longname description) acc =
      let
        l = pretty longname
        d = fromRope description
      in
        fillBreak 16 ("  " <> l <> " ") <+> align (reflow d) <> hardline <> acc
    g (Variable longname description) acc =
      let
        l = pretty longname
        d = fromRope description
      in
        fillBreak 16 ("  " <> l <> " ") <+> align (reflow d) <> hardline <> acc

    formatCommands :: [Commands] -> Doc ann
    formatCommands commands = hardline <> foldr h emptyDoc commands

    h :: Commands -> Doc ann -> Doc ann
    h (Command longname description _) acc =
      let
        l = pretty longname
        d = fromRope description
      in
        fillBreak 16 ("  " <> l <> " ") <+> align (reflow d) <> hardline <> acc
    h _ acc = acc

