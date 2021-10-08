{-# OPTIONS_HADDOCK not-home #-}

-- actually, they're there to group implementation too, but hey.

-- |
-- Support for building observability into your program by marking spans and
-- sub-spans, forming them into traces, and sending them to a backend capable
-- of ingesting such telemetry and performing analysis on it.
--
-- This is intended to be used directly:
--
-- @
-- import "Core.Telemetry"
-- @
--
-- the submodules are mostly there to group documentation.
module Core.Telemetry
  ( -- * Traces and Spans

    -- |
    -- Adding observability tracing to your program.
    module Core.Telemetry.Observability,
    
    -- * Exporting to backends

    -- |
    -- Processors to export telemetry to a backend.
    module Core.Telemetry.Backends,
  )
where

import Core.Telemetry.Observability
import Core.Telemetry.Backends
