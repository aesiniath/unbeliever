{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Dig metadata out of the description of your project.

This uses the evil /Template Haskell/ to run code at compile time that
parses the /.cabal/ file for your Haskell project and extracts various
meaningful fields.
-}
module Core.Program.Metadata
(
      Version
      {-* Splice -}
    , fromPackage
      {-* Internals -}
    , versionNumberFrom
    , projectNameFrom
    , projectSynopsisFrom
)
where

import Core.Data
import Core.Text
import Core.System (withFile, IOMode(..))
import Data.List (intersperse)
import qualified Data.List as List (isSuffixOf, find)
import Data.Maybe (fromMaybe)
import Data.String
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift, Exp(..))
import System.Directory (listDirectory)

{-|
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
data Version = Version {
      projectNameFrom :: String
    , projectSynopsisFrom :: String
    , versionNumberFrom :: String
} deriving (Show, Lift)

emptyVersion :: Version
emptyVersion = Version "" "" "0"

instance IsString Version where
    fromString x = emptyVersion { versionNumberFrom = x }

{-|
This is a splice which includes key built-time metadata, including the
number from the version field from your project's /.cabal/ file (as written
by hand or generated from /package.yaml/).

While we generally discourage the use of Template Haskell by beginners
(there are more important things to learn first) it is a way to execute
code at compile time and that is what what we need in order to have the
version number extracted from the /.cabal/ file rather than requiring the
user to specify (and synchronize) it in multiple places.

To use this, enable the Template Haskell language extension in your
/Main.hs/ file. Then use the special @$( ... )@ \"insert splice here\"
syntax that extension provides to get a 'Version' object with the desired
metadata about your project:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}

version :: 'Version'
version = $('fromPackage')

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' version 'Core.Program.Execute.None' ('Core.Program.Arguments.simple' ...
@

(Using Template Haskell slows down compilation of this file, but the upside
of this technique is that it avoids linking the Haskell build machinery
into your executable, saving you about 10 MB in the size of the resultant
binary)
-}
fromPackage :: Q Exp
fromPackage = do
    pairs <- readCabalFile

    let name = fromMaybe "" . lookupKeyValue "name" $ pairs
    let synopsis = fromMaybe "" . lookupKeyValue "synopsis" $ pairs
    let version = fromMaybe "" . lookupKeyValue "version" $ pairs

    let result = Version
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

parseCabalFile :: Bytes -> Map Rope Rope
parseCabalFile contents =
  let
    breakup = intoMap . fmap (breakRope (== ':')) . breakLines . fromBytes
  in
    breakup contents

-- this should probably be a function in Core.Text.Rope
breakRope :: (Char -> Bool) -> Rope -> (Rope,Rope)
breakRope predicate text =
  let
    pieces = take 2 (breakPieces predicate text)
  in
    case pieces of
        [] -> ("","")
        [one] -> (one,"")
        (one:two:_) -> (one, trimRope two)

-- knock off the whitespace in "name:      hello"
trimRope :: Rope -> Rope
trimRope = mconcat . intersperse " " . breakWords
