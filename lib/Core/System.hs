{-# OPTIONS_HADDOCK not-home #-}

{-|
Common elements from the rest of the Haskell ecosystem. This is mostly
about re-exports. There are numerous types and functions that are more or
less assumed to be in scope when you're doing much of anything in Haskell;
this module is a convenience to pull in the ones we rely on for the rest of
this library.

You can just import this directly:

@
import "Core.System"
@

as there's no particular benefit to cherry-picking the various sub-modules.

-}
module Core.System
    (
        {-* Base libraries -}
        module Core.System.Base
    ) where

import Core.System.Base

