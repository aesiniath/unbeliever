{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

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
import Distribution.Types.PackageDescription (synopsis, package)
import Distribution.Types.PackageId (pkgName, pkgVersion)
import Distribution.Types.PackageName (unPackageName)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Verbosity (normal)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift, Exp(..))
import System.Directory (listDirectory)

{-|
Information about the version number of this piece of software and other
related metadata related to the project it was built from. This is supplied to your
program when you call 'configure'. This value is used, along with the
proram name, if the user requests it by specifying the @--version@ option
on the command-line. You can also call 'getVersionNumber'.
FIXME
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
fromPackage = do
    generic <- readCabalFile
    let desc = packageDescription generic
        version = Version
            { projectNameFrom = unPackageName . pkgName . package $ desc
            , projectSynopsisFrom = synopsis desc
            , versionNumberFrom = prettyShow . pkgVersion . package $ desc
            }

--  I would have preferred
--
--  let e = AppE (VarE ...
--  return e
--
--  but that's not happening. So more voodoo TH nonsense instead.

    [e|version|]


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

readCabalFile :: Q GenericPackageDescription
readCabalFile = runIO $ do
    -- Find .cabal file
    file <- findCabalFile
    
    -- Parse .cabal file
    desc <- readGenericPackageDescription normal file

    -- pass to calling program
    return desc


