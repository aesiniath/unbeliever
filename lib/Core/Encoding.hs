{-# OPTIONS_HADDOCK not-home #-}

{-|
Various formats used for serialization, data transfer, and configuration.

This can be used by simply importing the top level module:

@
import "Core.Encoding"
@

although the individual formats are quite usable indepedently.

Each of these encodings are backed by a popular and well tuned library in
wide use across the Haskell community; these modules are here as wrappers
providing for ease of use and interoperability across the various tools in
this package.

-}
module Core.Encoding
    (
        module Core.Encoding.Json
    ) where

import Core.Encoding.Json

