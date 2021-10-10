{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
A backend exporter that sends telemetry in the form of traces of your
application's behaviour, or event data—accompanied either way by [conceivably
very wide] additional metadata—to the Honeycomb observability service.

/Notice/

This library is Open Source but the Honeycomb service is /not/. Honeycomb
offers a free tier which is quite suitable for individual use and small local
applications. You can also look at "Core.Telemetry.Other" if you instead want
to forward to a generic OpenTelemetry provider. There's also
"Core.Telemetry.Console" which simply dumps telemetry to console.
-}
module Core.Telemetry.Honeycomb (
    Dataset,
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

{- |
Indicate which \"dataset\" spans and events will be posted into
-}
type Dataset = Rope

{-|
Configure your application to send telemetry in the form of spans and traces
to the Honeycomb observability service.

You need to specify the \"dataset\" that your telemetry data will be posted
into.

@
    context <- configure ...
    context' <- initializeTelemetry (honeycombExporter "web-service-prod") context
    executeWith context' ...
@
-}
honeycombExporter :: Dataset -> Exporter
honeycombExporter _ =
    Exporter
        { codenameFrom = "honeycomb"
        , processorFrom = process
        }
  where
    process :: Datum -> IO Rope
    process datum = do
        let json = convertDatumToJson datum
        pure (render 80 json)

    convertDatumToJson :: Datum -> JsonValue
    convertDatumToJson datum =
        let meta0 = attachedMetadataFrom datum

            meta1 = insertKeyValue "name" (JsonString (spanNameFrom datum)) meta0

            meta2 = case spanIdentifierFrom datum of
                Nothing -> meta1
                Just self -> insertKeyValue "trace.span_id" (JsonString (unSpan self)) meta1

            meta3 = case parentIdentifierFrom datum of
                Nothing -> meta2
                Just parent -> insertKeyValue "trace.parent_id" (JsonString (unSpan parent)) meta2

            meta4 = case traceIdentifierFrom datum of
                Nothing -> meta3
                Just trace -> insertKeyValue "trace.trace_id" (JsonString (unTrace trace)) meta3

            meta5 = case serviceNameFrom datum of
                Nothing -> meta4
                Just service -> insertKeyValue "service_name" (JsonString service) meta4

            meta6 = case durationFrom datum of
                Nothing -> meta5
                Just duration ->
                    insertKeyValue
                        "duration_ms"
                        (JsonNumber (fromRational (toRational duration / 1e6)))
                        meta5

            json = JsonObject meta6
         in json
