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
{-|
Re-exports from foundational libraries supplied by the compiler runtime,
or from re-implementations of those areas.
-}
        module Core.System.Base

        {-* External dependencies -}
{-|
Dependencies from libraries outside the traditional ecosystem of Haskell.
These are typically special cases or custom re-implementations of things
which are maintained either by ourselves or people we are in regular
contact with.
-}
      , module Core.System.External
    ) where

import Core.System.Base
import Core.System.External

