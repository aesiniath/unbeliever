{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Support for testing programs written in the 'Program' @t@ type without acutally
standing up the full stand-alone application execution environment.
-}
module Core.Testing.Support
    (   testProgram
    ) where

import Core.Program.Execute (Program, configure)
import Core.Program.Arguments (blank)
import Core.Program.Unlift (subProgram)

testProgram :: forall a t. t -> Program t a -> IO a
testProgram user program = do
    context <- configure "0" user blank
    subProgram context program
