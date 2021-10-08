{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Implementations of different backends that telemetry can be exported to.
-}
module Core.Telemetry.Backends (
    Dataset,
    Exporter,
    consoleExporter,
    honeycombExporter,
) where

import Core.Data.Structures (Map, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (stdout)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Colour
import Core.Text.Rope
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)

-- TODO convert this into a Render instance

-- Somewhat counterintuitively, this does NOT do I/O, but instead returns text
-- which `processTelemetryMessages` will then forward to the main output queue
-- consumed by `processStandardOutput`. This is a bit roundabout, but ensures
-- debug output from this function doesn't smash the console.
consoleExporter :: Exporter
consoleExporter =
    Exporter
        { codenameFrom = "console"
        , processorFrom = \datum ->
            let text =
                    (intoEscapes brightBlue) <> "name: "
                        <> spanNameFrom datum
                        <> "\ntrace: "
                        <> case traceIdentifierFrom datum of
                            Nothing -> (intoEscapes pureRed) <> "[missing]" <> (intoEscapes brightBlue)
                            Just trace -> unTrace trace
                        <> "\nspan: "
                        <> case spanIdentifierFrom datum of
                            Nothing -> (intoEscapes pureRed) <> "[missing]" <> (intoEscapes brightBlue)
                            Just self -> unSpan self
                        <> "\nparent: "
                        <> case parentIdentifierFrom datum of
                            Nothing -> (intoEscapes dullYellow) <> "[none]" <> (intoEscapes brightBlue)
                            Just parent -> unSpan parent
                        <> "\nstart: "
                        <> intoRope (show (spanTimeFrom datum))
                        <> "\nduration: "
                        <> case durationFrom datum of
                            Nothing -> (intoEscapes dullYellow) <> "[none]" <> (intoEscapes brightBlue)
                            Just elapsed -> intoRope (show elapsed) <> " ns"
                        <> "\nmetadata:\n"
                        <> "FIXME"
                        <> (intoEscapes resetColour)
             in pure text
        }

{- |
Indicate which \"dataset\" spans and events will be posted into
-}
type Dataset = Rope

honeycombExporter :: Dataset -> Exporter
honeycombExporter _ = emptyExporter{codenameFrom = "honeycomb"}
