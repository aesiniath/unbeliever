{-|
Dig metadata out of the .cabal file of your project.

This uses the evil /Template Haskell/ to run code at compile time that
parses the .cabal file for your Haskell project and extracts various
meaningful fields.
-}
module Core.Program.Metadata
(
      Version(..)
    , fromPackage
)
where

import qualified Data.List as List
import Data.String
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.PackageDescription (synopsis)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import Language.Haskell.TH (Q, runIO, Exp(..), Lit(..))
import System.Directory (listDirectory)

import Core.Text.Rope

{-|
Information about the version number of this piece of software and other
related metadata related to the project it was built from. This is supplied to your
program when you call 'configure'. This value is used, along with the
proram name, if the user requests it by specifying the @--version@ option
on the command-line. You can also call 'getVersionNumber'.
FIXME
-}
data Version = Version {
      projectNameFrom :: Rope
    , projectVersionFrom :: Rope
    , projectSynopsisFrom :: Rope
}

emptyVersion :: Version
emptyVersion = Version mempty mempty mempty

instance IsString Version where
    fromString x = emptyVersion { projectVersionFrom = intoRope x }

{-|
This is a splice which includes key built-time metadata, including the
number from the version field from your project's /.cabal/ file (as written
by hand or generated from /package.yaml/).

While we generally discourage the use of Template Haskell by beginners
(there are more important things to learn first) it is a way to execute
code at compile time and that is what what we need in order to have the
version number extracted from the /.cabal/ file rather than requiring the
user to specify (and synchornize) it in multiple places.

To use this, enable the Template Haskell language extension in your
/Main.hs/ file:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
@

Then use the special @$( ... ) "insert splice here" syntax that extension
provides to get a 'Version' object with the desired metadata about your
project:

@
version :: 'Version'
version = $('fromPackage')

main :: IO ()
main = do
    context <- 'configure' version 'None' ('simple' ...
@

(this wraps the extensive machinery in the __Cabal__ library, notably
'PackageDescription'. The upside of this technique is that it avoids
linking the Haskell build machinery into your executable, saving you about
10 MB)
-}
fromPackage :: Q Exp
fromPackage = projectSynopsis1


findCabalFile :: IO FilePath
findCabalFile = do
    files <- listDirectory "."
    let found = List.find (List.isSuffixOf ".cabal") files
    case found of
        Just file -> return file
        Nothing -> error "No .cabal file found"

readCabalFile :: Q GenericPackageDescription
readCabalFile = runIO $ do
    -- Find .cabal file
    file <- findCabalFile
    
    -- Parse .cabal file
    desc <- readGenericPackageDescription normal file

    -- pass to calling program
    return desc


projectSynopsis1 :: Q Exp
projectSynopsis1 = do
    desc <- readCabalFile
    return ((LitE . StringL . synopsis . packageDescription) desc)

