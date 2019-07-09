{-# OPTIONS_HADDOCK not-home #-}

{-|
Wrappers and adaptors for various data structures common in the Haskell
ecosystem.

This is intended to be used directly:

@
import "Core.Data"
@

as this module re-exports all of its various components.
-}
module Core.Data
    (
        {-* Wrappers -}
{-|
Exposes 'Map', a wrapper around a dictionary type, and 'Set', for
collections of elements.
-}
        module Core.Data.Structures
    ) where

import Core.Data.Structures

