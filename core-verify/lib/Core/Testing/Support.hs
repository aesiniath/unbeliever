{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Support for testing programs written in the 'Program' @τ@ type without
acutally standing up the full stand-alone application execution
environment.
-}
module Core.Testing.Support
    (   testProgram
    ) where

import Core.Program.Execute (Program, configure)
import Core.Program.Arguments (blank)
import Core.Program.Unlift (subProgram)

{-|
Run a 'Program' @τ@ and return the result.

This sets up a minimal context that will not actually output or log
anything but which can be used to evaluate a Program action in 

/This is preliminary; while the function signature will remain stable we/
/have considerable work to do for this to be idempotent, reusable, and/
/safe./
-}
-- TODO better isolation! Calling configure does way to much actual work.
testProgram :: forall α τ. τ -> Program τ α -> IO α
testProgram user program = do
    context <- configure "0" user blank
    subProgram context program
