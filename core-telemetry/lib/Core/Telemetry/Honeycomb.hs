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

import Core.Data.Structures (Map, fromMap, insertKeyValue, intoMap)
import Core.Encoding.Json
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (stdout)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Bytes
import Core.Text.Colour
import Core.Text.Rope
import Core.Text.Utilities
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as C (append, null, putStrLn)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Fixed
import qualified Data.List as List
import Network.Http.Client
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.Posix.Process as Posix (exitImmediately)

{- |
Indicate which \"dataset\" spans and events will be posted into
-}
type Dataset = Rope

type ApiKey = Rope

{- |
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

-- so this is annoying: we're _under_ (and indeed, before) the Program monad
-- and in the guts of the library. So all the work we've done to provide
-- sensible access to environment variables etc isn't available here and we
-- have to replicate a bunch of stuff we've done elsewhere.
honeycombExporter :: Dataset -> IO Exporter
honeycombExporter dataset = do
    possible <- lookupEnv "HONEYCOMB_TEAM"

    apikey <- case possible of
        Nothing -> do
            putStrLn "error: HONEYCOMB_TEAM environment variable not set with API key"
            Posix.exitImmediately (ExitFailure 99)
            undefined
        Just value -> pure (packRope value)

    pure
        ( Exporter
            { codenameFrom = "honeycomb"
            , processorFrom = process apikey dataset
            }
        )

process :: ApiKey -> Dataset -> Datum -> IO Rope
process apikey dataset datum = do
    let json = convertDatumToJson datum

    postEventToHoneycombAPI apikey dataset json

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

        -- start = show (fromRational (toRational (spanTimeFrom datum) / 1e9) :: Fixed E9)
        -- meta7 = insertKeyValue "timestamp" (JsonString (intoRope (show (spanTimeFrom datum)))) meta6
        time = intoRope (show (spanTimeFrom datum))
        point =
            JsonObject
                ( intoMap
                    [ (JsonKey "time", JsonString time)
                    , (JsonKey "data", JsonObject meta6)
                    ]
                )
     in JsonArray [point]

postEventToHoneycombAPI :: ApiKey -> Dataset -> JsonValue -> IO ()
postEventToHoneycombAPI apikey dataset json = do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx "api.honeycomb.io" 443

    let q = buildRequest1 $ do
            http POST (C.append "/1/batch/" (fromRope dataset))
            setContentType "application/json"
            setHeader "X-Honeycomb-Team" (fromRope (apikey))

    sendRequest c q (simpleBody (fromBytes (encodeToUTF8 json)))
    result <- receiveResponse c simpleHandler

    -- this is rubbish error handling. We'll clean this up! we're not,
    -- however, going to crash out.
    if (C.null result)
        then pure ()
        else do
            putStr "Failed to post to Honeycomb: "
            C.putStrLn result
