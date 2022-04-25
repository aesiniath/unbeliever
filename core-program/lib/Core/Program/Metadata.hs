{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Digging metadata out of the description of your project, and other useful
helpers.
-}
module Core.Program.Metadata (
    Version,
    versionNumberFrom,
    projectNameFrom,
    projectSynopsisFrom,

    -- * Splice
    fromPackage,

    -- * Source code
    __LOCATION__,
) where

import Core.Data
import Core.System.Base (IOMode (..), withFile)
import Core.System.Pretty
import Core.Text
import qualified Data.List as List (find, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Exp (..), Lift)
import System.Directory (listDirectory)

{- |
Information about the version number of this piece of software and other
related metadata related to the project it was built from. This is supplied
to your program when you call 'Core.Program.Execute.configure'. This value
is used if the user requests it by specifying the @--version@ option on the
command-line.

Simply providing an overloaded string literal such as version @\"1.0\"@
will give you a 'Version' with that value:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' \"1.0\" 'Core.Program.Execute.None' ('Core.Program.Arguments.simple' ...
@

For more complex usage you can populate a 'Version' object using the
'fromPackage' splice below. You can then call various accessors like
'versionNumberFrom' to access individual fields.
-}
data Version = Version
    { projectNameFrom :: String
    , projectSynopsisFrom :: String
    , versionNumberFrom :: String
    }
    deriving (Show, Lift)

emptyVersion :: Version
emptyVersion = Version "" "" "0"

instance IsString Version where
    fromString x = emptyVersion{versionNumberFrom = x}

{- |
This is a splice which includes key built-time metadata, including the number
from the version field from your project's /.cabal/ file (as written by hand
or generated from /package.yaml/). This uses the evil @TemplateHaskell@
extension.

While we generally discourage the use of Template Haskell by beginners (there
are more important things to learn first) it is a way to execute code at
compile time and that is what what we need in order to have the version number
extracted from the /.cabal/ file rather than requiring the user to specify
(and synchronize) it in multiple places.

To use this, enable the Template Haskell language extension in your /Main.hs/
file. Then use the special @$( ... )@ \"insert splice here\" syntax that
extension provides to get a 'Version' object with the desired metadata about
your project:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}

version :: 'Version' version = $('fromPackage')

main :: 'IO' () main = do context <- 'Core.Program.Execute.configure' version
'Core.Program.Execute.None' ('Core.Program.Arguments.simple' ...
@

(Using Template Haskell slows down compilation of this file, but the upside of
this technique is that it avoids linking the Haskell build machinery into your
executable, saving you about 10 MB in the size of the resultant binary)
-}
fromPackage :: Q Exp
fromPackage = do
    pairs <- readCabalFile

    let name = fromMaybe "" . lookupKeyValue "name" $ pairs
    let synopsis = fromMaybe "" . lookupKeyValue "synopsis" $ pairs
    let version = fromMaybe "" . lookupKeyValue "version" $ pairs

    let result =
            Version
                { projectNameFrom = fromRope name
                , projectSynopsisFrom = fromRope synopsis
                , versionNumberFrom = fromRope version
                }

    --  I would have preferred
    --
    --  let e = AppE (VarE ...
    --  return e
    --
    --  but that's not happening. So more voodoo TH nonsense instead.

    [e|result|]

{-
Locate the .cabal file in the present working directory (assumed to be the
build root) and use the **Cabal** library to parse the few bits we need out
of it.
-}

findCabalFile :: IO FilePath
findCabalFile = do
    files <- listDirectory "."
    let found = List.find (List.isSuffixOf ".cabal") files
    case found of
        Just file -> return file
        Nothing -> error "No .cabal file found"

readCabalFile :: Q (Map Rope Rope)
readCabalFile = runIO $ do
    -- Find .cabal file
    file <- findCabalFile

    -- Parse .cabal file
    contents <- withFile file ReadMode hInput
    let pairs = parseCabalFile contents
    -- pass to calling program
    return pairs

-- TODO this could be improved; we really only need the data from the first
-- block of lines, with colons in them! We're probably reached the point where
-- a proper parser would be good, but whatever.
parseCabalFile :: Bytes -> Map Rope Rope
parseCabalFile contents =
    let breakup = intoMap . fmap (\(a, b) -> (a, trimValue b)) . fmap (breakRope (== ':')) . breakLines . fromBytes
     in breakup contents

-- knock off the colon and whitespace in ":      hello"
trimValue :: Rope -> Rope
trimValue value = case unconsRope value of
    Nothing -> emptyRope
    Just (_, remainder) -> case findIndexRope (/= ' ') remainder of
        Nothing -> emptyRope
        Just i -> snd (splitRope i remainder)

{- |
Access the source location of the call site.

This is insanely cool, and does /not/ require you to turn on the @CPP@ or
@TemplateHaskell@ language extensions! Nevertheless we named it with
underscores to compliment the symbols that @CPP@ gives you; the double
underscore convention holds across many languages and stands out as a very
meta thing, even if it is a proper Haskell value.

We have a 'Render' instance that simply prints the filename and line number.
Doing:

@
main :: 'IO' ()
main = 'Core.Program.Execute.execute' $ do
    'Core.Program.Logging.writeR' '__LOCATION__'
@

will give you:

@
tests/Snipppet.hs:32
@

This isn't the full stack trace, just information about the current line. If
you want more comprehensive stack trace you need to add 'HasCallStack'
constraints everywhere, and then...
-}

-- This works because the call stack has the most recent frame at the head of
-- the list. Huge credit to Matt Parsons for having pointed out this technique
-- at <https://twitter.com/mattoflambda/status/1460769133923028995>

__LOCATION__ :: HasCallStack => SrcLoc
__LOCATION__ =
    case getCallStack callStack of
        (_, srcLoc) : _ -> srcLoc
        _ -> emptySrcLoc
  where
    -- we construct a dud SrcLoc rather than using error "unreachable!"
    -- because often the only time you need a source location is an exception
    -- pathway already. If something goes wrong with this gimick we don't want
    -- to submerge the actual problem.
    emptySrcLoc =
        SrcLoc
            { srcLocPackage = ""
            , srcLocModule = ""
            , srcLocFile = ""
            , srcLocStartLine = 0
            , srcLocStartCol = 0
            , srcLocEndLine = 0
            , srcLocEndCol = 0
            }

instance Render SrcLoc where
    type Token SrcLoc = ()
    colourize = const pureWhite
    highlight loc =
        pretty (srcLocFile loc)
            <> ":"
            <> pretty (show (srcLocStartLine loc))
