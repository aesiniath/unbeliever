{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
An exporter backend that outputs \"structured\" logs to your terminal as they are
submitted.

Most traditional programs' logs were textual, single lines, sometimes with a
reasonably well-known internal layout, but regardless very difficult to
actually perform analysis on. Engineers attempting to diagnose problems are
largely limited to doing text searches across masses of logs. It's hard to
correlate between diffent subsystems let alone perform any sort of statistical
analysis.

Other systems in the past gathered copious amounts of metrics but having done
so, left us with the hard problem of actually doing anything useful with them
other than providing fodder for pretty graphs.

Structured logging was a significant step forward for large-scale systems
administration; by combining metrics together with context in the form of
key/value pairs it allows us to perform more detailed investigation and
analysis that this was largely done by emitting copious amounts of enormously
wasteful JSON is astonishing and goes some way to explain why structured
logging took so long to catch on).

Taking the example from 'Core.Telemetry.Observability.telemetry', the output
would be:

@
\$ __burgerservice --telemetry=structured__
{"calories":667.0,"flavour":true,"meal_name":"hamburger","precise":45.0,"timestamp":"2021-10-22T11:12:53.674399531Z"}
...
@

which if pretty printed would have been more recognizable as

@
{
    "calories": 667.0,
    "flavour": true,
    "meal_name": "hamburger",
    "precise": 45.0,
    "timestamp": "2021-10-22T11:12:53.674399531Z",
}
@

but all that whitespace would be wasteful, right?

While more advanced observability systems will directly ingest this data and
assemble it into traces of nested spans, in other situations having
straight-forward metrics output as JSON may be sufficient for your needs. If
you /do/ use this exporter in a program embellished with traces and spans, the
relevant contextual information will be added to the output:

@
{
    "calories": 667.0,
    "flavour": true,
    "meal_name": "hamburger",
    "precise": 45.0,
    "timestamp": "2021-10-22T11:12:53.674399531Z",
    "duration": 3.756717001,
    "span_id": "o7ucNqCeSJBzeviL",
    "span_name": "Process order",
    "trace_id": "order-11430185",
    "service_name": "burger-service"
}
@
-}
module Core.Telemetry.Structured (
    structuredExporter,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Core.Data.Structures (insertKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Text.Rope

{- |
Output metrics to @stdout@ in the form of a raw JSON object.
-}
structuredExporter :: Exporter
structuredExporter =
    Exporter
        { codenameFrom = "structured"
        , setupConfigFrom = setupStructuredConfig
        , setupActionFrom = setupStructuredAction
        }

setupStructuredConfig :: Config -> Config
setupStructuredConfig = id

setupStructuredAction :: Context τ -> IO Forwarder
setupStructuredAction context = do
    let out = outputChannelFrom context
    pure
        ( Forwarder
            { telemetryHandlerFrom = processStructuredOutput out
            }
        )

-- almost exact copy of what we use to send to Honeycomb
convertDatumToJson :: Datum -> JsonValue
convertDatumToJson datum =
    let spani = spanIdentifierFrom datum
        trace = traceIdentifierFrom datum
        parent = parentIdentifierFrom datum
        meta0 = attachedMetadataFrom datum

        meta1 = insertKeyValue "span_name" (JsonString (spanNameFrom datum)) meta0

        meta2 = case spani of
            Nothing -> meta1
            Just value -> insertKeyValue "span_id" (JsonString (unSpan value)) meta1

        meta3 = case parent of
            Nothing -> meta2
            Just value -> insertKeyValue "parent_id" (JsonString (unSpan value)) meta2

        meta4 = case trace of
            Nothing -> meta3
            Just value -> insertKeyValue "trace_id" (JsonString (unTrace value)) meta3

        meta5 = case serviceNameFrom datum of
            Nothing -> meta4
            Just service -> insertKeyValue "service_name" (JsonString service) meta4

        meta6 = case durationFrom datum of
            Nothing -> meta5
            Just duration ->
                insertKeyValue
                    "duration"
                    (JsonNumber (fromRational (toRational duration / 1e9)))
                    meta5

        time = intoRope (show (spanTimeFrom datum))
        meta7 = insertKeyValue "timestamp" (JsonString time) meta6
     in JsonObject meta7

processStructuredOutput :: TQueue (Maybe Rope) -> [Datum] -> IO ()
processStructuredOutput out datums = do
    mapM_ processOne datums
  where
    processOne :: Datum -> IO ()
    processOne datum = do
        let object = convertDatumToJson datum
            text = intoRope (encodeToUTF8 object)

        atomically $ do
            writeTQueue out (Just text)
