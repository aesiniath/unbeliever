{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Implementations of different backends that telemetry can be exported to.
-}
module Core.Telemetry.Backends (
    Exporter,
    debugExporter,
    honeycomb,
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
import Data.Maybe (fromMaybe)

debugExporter :: Exporter
debugExporter =
    Exporter
        { processorFrom = \datum ->
            let text =
                    (intoEscapes brightBlue) <> "name: "
                        <> spanNameFrom datum
                        <> "\ntrace: "
                        <> case traceIdentifierFrom datum of
                            Nothing -> (intoEscapes pureRed) <> "??? no trace" <> (intoEscapes brightBlue)
                            Just trace -> unTrace trace
                        <> "\nspan: "
                        <> unSpan (spanIdentifierFrom datum)
                        <> "\nparent: "
                        <> case parentIdentifierFrom datum of
                            Nothing -> (intoEscapes dullYellow) <> "[none]" <> (intoEscapes brightBlue)
                            Just parent -> unSpan parent
                        <> "\nstart: "
                        <> intoRope (show (spanTimeFrom datum))
                        <> "\nduration: "
                        <> case durationFrom datum of
                            Nothing -> "[no duration]"
                            Just elapsed -> intoRope (show elapsed) <> " ns"
                        <> "\nmetadata:\n"
                        <> "FIXME"
                        <> (intoEscapes resetColour)
             in pure text
        }

honeycomb :: Rope -> Exporter
honeycomb _ = emptyExporter
