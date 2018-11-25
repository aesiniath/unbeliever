{-|
Dig metadata out of the .cabal file of your project.

This uses the evil /Template Haskell/ to run code at compile time that
parses the .cabal file for your Haskell project and extracts various
meaningful fields.
-}
module Core.Program.Metadata where

import qualified Data.List as List
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.PackageDescription (synopsis)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import Language.Haskell.TH (Q, runQ, runIO, Exp(..), Lit(..))
import System.Directory (listDirectory)


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

