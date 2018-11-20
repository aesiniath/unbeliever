module Core.Program.Metadata where

import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.PackageDescription (synopsis)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import Language.Haskell.TH (Q, runQ, runIO)

readCabalFile :: Q GenericPackageDescription
readCabalFile = runIO $ do
    -- Find .cabal file
    let file = "unbeliever.cabal"
    
    -- Parse .cabal file
    desc <- readGenericPackageDescription normal file

    -- pass to calling program
    return desc


projectSynopsis :: IO String
projectSynopsis = do
    desc <- runQ readCabalFile
    return (synopsis (packageDescription desc))
