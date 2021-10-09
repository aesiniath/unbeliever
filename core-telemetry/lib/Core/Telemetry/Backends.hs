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

import Core.Data.Structures (Map, fromMap, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (stdout)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Colour
import Core.Text.Rope
import Core.Text.Utilities
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
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
        , processorFrom = \datum ->
            let text =
                    (intoEscapes dullBlue) <> "name: "
                        <> spanNameFrom datum
                        <> "\ntrace: "
                        <> case traceIdentifierFrom datum of
                            Nothing -> (intoEscapes pureRed) <> "[missing]" <> (intoEscapes dullBlue)
                            Just trace -> unTrace trace
                        <> "\nspan: "
                        <> case spanIdentifierFrom datum of
                            Nothing -> (intoEscapes pureRed) <> "[missing]" <> (intoEscapes dullBlue)
                            Just self -> unSpan self
                        <> "\nparent: "
                        <> case parentIdentifierFrom datum of
                            Nothing -> (intoEscapes dullYellow) <> "[none]" <> (intoEscapes dullBlue)
                            Just parent -> unSpan parent
                        <> "\nstart: "
                        <> intoRope (show (spanTimeFrom datum))
                        <> "\nduration: "
                        <> case durationFrom datum of
                            Nothing -> (intoEscapes dullYellow) <> "[none]" <> (intoEscapes dullBlue)
                            Just elapsed -> intoRope (show elapsed) <> " ns"
                        <> "\nmetadata:"
                        <> let pairs :: [(JsonKey, JsonValue)]
                               pairs = fromMap (attachedMetadataFrom datum)
                            in List.foldl' f emptyRope pairs
                                <> (intoEscapes resetColour)
             in pure text
        }
  where
    f :: Rope -> (JsonKey, JsonValue) -> Rope
    f acc (k, v) =
        acc <> "\n  "
            <> (intoEscapes dullBlue)
            <> render 80 k
            <> (intoEscapes dullBlue)
            <> " = "
            <> render 80 v

{- |
Indicate which \"dataset\" spans and events will be posted into
-}
type Dataset = Rope

honeycombExporter :: Dataset -> Exporter
honeycombExporter _ = emptyExporter{codenameFrom = "honeycomb"}
