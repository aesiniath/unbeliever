{-# OPTIONS_HADDOCK not-home #-}

{-|
Meta package re-exporting all the modules in the collection.
-}
module Core
    (
        module Core.Text
      , module Core.Program
      , module Core.Data
      , module Core.Encoding
      , module Core.System
    ) where

import Core.Text
import Core.Program
import Core.System
import Core.Data
import Core.Encoding

