{-# OPTIONS_HADDOCK not-home #-}

{- |
Wrappers and adaptors for various data structures common in the Haskell
ecosystem.

This is intended to be used directly:

@
import "Core.Data"
@

as this module re-exports all of its various components.
-}
module Core.Data
    ( -- * Wrappers

      -- |
      -- Exposes 'Map', a wrapper around a dictionary type, and 'Set', for
      -- collections of elements.
        module Core.Data.Structures
      -- |
      -- Facilities for making timestamps and for converting between different representations of instants of time.
    , module Core.Data.Clock
    ) where

import Core.Data.Clock
import Core.Data.Structures
