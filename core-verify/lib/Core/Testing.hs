{-# OPTIONS_HADDOCK not-home #-}

{-|
Testing support for programs written using __core-program__.

This is intended to be used directly:

@
import "Core.Testing"
@

Submodules are mostly used to group documentation.
-}
module Core.Testing
    (
        {-* Testing a program -}
{-|
Execute a 'Program' @t@ without acutally standing up the full application
execution environment.
-}
        module Core.Testing.Support
    ) where

import Core.Testing.Support
