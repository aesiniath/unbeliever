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

import Core.Data.Structures (Map, fromMap, insertKeyValue, intoMap, lookupKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
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

@
    context <- configure ...
    context' <- initializeTelemetry honeycombExporter context
    executeWith context' ...
@
-}
honeycombExporter :: Exporter
honeycombExporter =
    Exporter
        { codenameFrom = "honeycomb"
        , setupConfigFrom = setupHoneycombConfig
        , setupActionFrom = setupHoneycombAction
        }

-- so this is annoying: we're _under_ (and indeed, before) the Program monad
-- and in the guts of the library. So all the work we've done to provide
-- sensible access to environment variables etc isn't available here and we
-- have to replicate a bunch of stuff we've done elsewhere.

setupHoneycombConfig :: Config -> Config
setupHoneycombConfig config0 =
    let config1 =
            appendOption
                ( Variable
                    "HONEYCOMB_TEAM"
                    "The API key used to permit writes to Honeycomb."
                )
                config0

        config2 =
            appendOption
                ( Option
                    "dataset"
                    Nothing
                    (Value "DATASET")
                    "The name of the dataset within your Honeycomb account that this program's telemetry will be written to."
                )
                config1
     in config2

setupHoneycombAction :: Context τ -> IO Forwarder
setupHoneycombAction context = do
    let params = commandLineFrom context
        pairs = environmentValuesFrom params
        possibleTeam = lookupKeyValue "HONEYCOMB_TEAM" pairs

    apikey <- case possibleTeam of
        Nothing -> do
            putStrLn "error: Need to supply an API key in the HONEYCOMB_TEAM environment variable."
            Posix.exitImmediately (ExitFailure 99)
            undefined
        Just param -> case param of
            Empty -> do
                putStrLn "error: Need to actually supply a value in HONEYCOMB_TEAM environment variable."
                Posix.exitImmediately (ExitFailure 99)
                undefined
            Value value -> pure (intoRope value)

    let options = parameterValuesFrom params
        possibleDataset = lookupKeyValue "dataset" options

    dataset <- case possibleDataset of
        Nothing -> do
            putStrLn "error: Need to specify the dataset that metrics will be written to via --dataset."
            Posix.exitImmediately (ExitFailure 99)
            undefined
        Just param -> case param of
            Empty -> do
                putStrLn "error: Need to actually supply a value to the --dataset option."
                Posix.exitImmediately (ExitFailure 99)
                undefined
            Value "" -> do
                putStrLn "error: Need to actually supply a value to the --dataset option."
                Posix.exitImmediately (ExitFailure 99)
                undefined
            Value value -> pure (intoRope value)

    pure
        Forwarder
            { telemetryHandlerFrom = process apikey dataset
            }

-- use partually applied
process :: ApiKey -> Dataset -> Datum -> IO ()
process apikey dataset datum = do
    let json = convertDatumToJson datum
    postEventToHoneycombAPI apikey dataset json

convertDatumToJson :: Datum -> JsonValue
convertDatumToJson datum =
    let meta0 = attachedMetadataFrom datum

        meta1 = insertKeyValue "name" (JsonString (spanNameFrom datum)) meta0

        meta2 = case spanIdentifierFrom datum of
            Nothing -> insertKeyValue "meta.annotation_type" (JsonString "span_event") meta1
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

    putStrLn (fromRope ("Processing " <> render 80 json))

    sendRequest c q (simpleBody (fromBytes (encodeToUTF8 json)))
    result <- receiveResponse c simpleHandler

    -- this is rubbish error handling. We'll clean this up! we're not,
    -- however, going to crash out.
    if (C.null result)
        then pure ()
        else do
            -- putStr "Failed to post to Honeycomb: "
            C.putStrLn "Response"
            C.putStrLn result
