{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Implementations of different backends that telemetry can be exported to.
-}
module Core.Telemetry.Console (
    Exporter,
    consoleExporter,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Core.Data.Structures (fromMap)
import Core.Encoding.Json
import Core.Program.Context
import Core.Program.Logging
import Core.System.External (getCurrentTimeNanoseconds)
import Core.Telemetry.Internal
import Core.Text.Colour
import Core.Text.Rope
import Core.Text.Utilities
import qualified Data.List as List

-- TODO convert this into a Render instance

-- Somewhat counterintuitively, this does NOT do I/O, but instead returns text
-- which `processTelemetryMessages` will then forward to the main output queue
-- consumed by `processStandardOutput`. This is a bit roundabout, but ensures
-- debug output from this function doesn't smash the console.
consoleExporter :: Exporter
consoleExporter =
    Exporter
        { codenameFrom = "console"
        , setupActionFrom = setup
        }

setup :: Context τ -> IO Forwarder
setup context = do
    let out = outputChannelFrom context
    pure
        ( Forwarder
            { telemetryHandlerFrom = process out
            }
        )

process :: TQueue Rope -> Datum -> IO ()
process out datum = do
    now <- getCurrentTimeNanoseconds
    let start = spanTimeFrom datum
    let text =
            (intoEscapes pureGrey)
                <> spanNameFrom datum
                <> " metrics:"
                <> let pairs :: [(JsonKey, JsonValue)]
                       pairs = fromMap (attachedMetadataFrom datum)
                    in List.foldl' f emptyRope pairs
                        <> (intoEscapes resetColour)

    let result = formatLogMessage start now SeverityDebug text
    atomically (writeTQueue out result)

f :: Rope -> (JsonKey, JsonValue) -> Rope
f acc (k, v) =
    acc <> "\n  "
        <> (intoEscapes pureGrey)
        <> render 80 k
        <> (intoEscapes pureGrey)
        <> " = "
        <> render 80 v
