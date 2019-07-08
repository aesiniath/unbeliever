{-# OPTIONS_HADDOCK not-home #-}

{-|
A unified Text type providing interoperability between various text
back-ends present in the Haskell ecosystem.

This is intended to be used directly:

@
import "Core.Text"
@

as this module re-exports all of the various components making up this
library's text handling subsystem.
-}
module Core.Text
    (
        {-* Internal representation -}
{-|
Exposes 'Bytes', a wrapper around different types of binary data, and 'Rope',
a finger-tree over buffers containing text.
-}
        module Core.Text.Bytes
      , module Core.Text.Rope

        {-* Useful utilities -}
{-|
Useful functions for common use cases.
-}
      , module Core.Text.Utilities
    ) where

import Core.Text.Bytes
import Core.Text.Rope
import Core.Text.Utilities

