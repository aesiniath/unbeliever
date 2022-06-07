{-# OPTIONS_HADDOCK not-home #-}

-- actually, they're there to group implementation too, but hey.

-- |
-- Support for building command-line programs, ranging from simple tools to
-- long-running daemons.
--
-- This is intended to be used directly:
--
-- @
-- import "Core.Program"
-- @
--
-- the submodules are mostly there to group documentation.
module Core.Program
  ( -- * Executing a program

    -- |
    -- A top-level Program type giving you unified access to logging, concurrency,
    -- and more.
    module Core.Program.Context,
    module Core.Program.Execute,
    module Core.Program.Threads,
    module Core.Program.Unlift,
    module Core.Program.Metadata,
    module Core.Program.Exceptions,

    -- * Command-line argument parsing

    -- |
    -- Including declaring what options your program accepts, generating help, and
    -- for more complex cases [sub]commands, mandatory arguments, and environment
    -- variable handling.
    module Core.Program.Arguments,

    -- * Logging facilities

    -- |
    -- Facilities for noting events through your program and doing debugging.
    module Core.Program.Logging,

    -- |
    -- There are a few common use cases which require a bit of wrapping to use
    -- effectively. Watching files for changes and taking action in the event of a
    -- change is one.
    module Core.Program.Notify,
  )
where

import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Exceptions
import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Metadata
import Core.Program.Notify
import Core.Program.Threads
import Core.Program.Unlift
