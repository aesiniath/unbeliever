{-# OPTIONS_HADDOCK not-home #-}

{-|
Support for building command-line programs, ranging from simple tools
to long-running daemons.
-}
module Core.Program
    (
      module Core.Program.Arguments
    , module Core.Program.Logging
    , module Core.Program.Execute
    ) where

import Core.Program.Arguments
import Core.Program.Logging
import Core.Program.Execute

