{-# OPTIONS_HADDOCK not-home #-}

{-|
Support for building command-line programs, ranging from simple tools to
long-running daemons.

This is intended to be used directly:

@
import "Core.Program"
@

the submodules are mostly there to group documentation.
-}
-- actually, they're there to group implementation too, but hey.
module Core.Program
    (
        {-* Executing a program -}
{-|
A top-level Program type giving you unified access to logging, concurrency,
and more.
-}
        module Core.Program.Execute
      , module Core.Program.Unlift
      , module Core.Program.Metadata

        {-* Command-line argument parsing -}
{-|
Including declaring what options your program accepts, generating help, and
for more complex cases [sub]commands, mandatory arguments, and environment
variable handling.
-}
      , module Core.Program.Arguments
        {-* Logging facilities -}
{-|
Facilities for noting events through your program and doing debugging.
-}
      , module Core.Program.Logging
    ) where

import Core.Program.Arguments
import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Metadata
import Core.Program.Unlift

