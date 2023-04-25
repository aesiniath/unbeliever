{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
A exporter backend that sends telemetry in the form of traces of your
application's behaviour, or event data—accompanied either way by [conceivably
very wide] additional metadata—to the Honeycomb observability service.

When specifying the 'honeycombExporter' you have to specify certain
command-line options and environment variables to enable it:

@
\$ __export HONEYCOMB_TEAM="62e3626a2cc34475adef4d799eca0407"__
\$ __burger-service --telemetry=honeycomb --dataset=prod-restaurant-001__
@

If you annotate your program with spans, you can get a trace like this:

![Example Trace](HoneycombTraceExample.png)

This library by default will upload telemetry information to the default
Honeycomb endpoint at 'api.honeycomb.io'. However, it also offers support for
intermediate services (such as Honeycomb Refinery) when specifying a host
explicitly, such as:

@
\$ __export HONEYCOMB_HOST=my-intermediate-service.internal__
@

The library still assumes that the service is running on port 443 and
behind SSL.

More details on Refinery: <https://docs.honeycomb.io/manage-data-volume/refinery/>

/Notice/

This library is Open Source but the Honeycomb service is /not/. Honeycomb
offers a free tier which is quite suitable for individual use and small local
applications. In the future you may be able to look at
"Core.Telemetry.General" if you instead want to forward to a generic
OpenTelemetry provider.

= Gotchas

Spans are sent to Honeycomb as they are closed. Hence, if you have a long
lived span, while its child spans are sent to Honeycomb and are displayed, the
parent span will be initially missing.

![Example Sad Trace](honeycomb-sad-trace.png)

This is of course jarring, because the parent is defined in the code /before/
the section where the child is called. So when writing long lived services, it
is best to call 'Core.Telemetry.Observability.beginTrace' inside a function
that will iterate continuously. That way complete telemetry will be generated
for that part of the code, making on-the-fly diagnosis and monitoring
possible.

Either way, when the parent span is closed, unless the process is killed, the
full trace will be visible.

![Example Happy Trace](honeycomb-happy-trace.png)
-}
module Core.Telemetry.Honeycomb
    ( Dataset
    , honeycombExporter
    , setDatasetName
    ) where

import Codec.Compression.GZip qualified as GZip (compress)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Exception.Safe qualified as Safe (catch, finally, throw)
import Control.Monad (forM_)
import Core.Data.Clock (Time, getCurrentTimeNanoseconds, unTime)
import Core.Data.Structures (Map, emptyMap, fromMap, insertKeyValue, intoMap, lookupKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (SomeException, liftIO, stdout)
import Core.Text.Bytes
import Core.Text.Colour
import Core.Text.Rope
import Core.Text.Utilities
import Data.ByteString (ByteString)
import Data.ByteString qualified as B (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder (lazyByteString)
import Data.ByteString.Char8 qualified as C (append, null, pack, putStrLn)
import Data.ByteString.Lazy qualified as L (ByteString)
import Data.Fixed
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Network.Http.Client
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams qualified as Streams (write)
import System.Posix.Process qualified as Posix (exitImmediately)

{- |
Indicate which \"dataset\" spans and events will be posted into
-}
type Dataset = Rope

type ApiKey = Rope

{- |
Configure your application to send telemetry in the form of spans and traces
to the Honeycomb observability service.

@
    context <- 'Core.Program.Execute.configure' ...
    context' <- 'Core.Telemetry.Observability.initializeTelemetry' ['honeycombExporter'] context
    'Core.Program.Execute.executeWith' context' ...
@
-}
honeycombExporter :: Exporter
honeycombExporter =
    Exporter
        { codenameFrom = "honeycomb"
        , setupConfigFrom = setupHoneycombConfig
        , setupActionFrom = setupHoneycombAction
        }

{- |
Override the dataset being used for telemetry.

Under normal circumstances this shouldn't be necessary. The default dataset
for your program's telemetry is set by the infrastructure using the
@--dataset=@ command-line option, and typically matches the single service
name set by 'setServiceName'. For most applications this is sufficient. There
are, however, times when you need to send events or spans to a /different/
dataset. If there are two completely unrelated behaviours in a given
application that occur with wildly different latency ranges then you /may/
find it appropriate to segment the telemetry into two different datasets.

This override will be inherited by any spans that come into scope below the
one where this is called.

@since 0.2.9
-}
setDatasetName :: Dataset -> Program τ ()
setDatasetName dataset = do
    context <- getContext

    liftIO $ do
        -- get the map out
        let v = currentDatumFrom context
        modifyMVar_
            v
            (\datum -> pure datum {datasetFrom = Just dataset})

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

        config3 =
            appendOption
                ( Variable
                    "HONEYCOMB_HOST"
                    "Override the default API endpoint for occasions where telemetry needs to be proxied through an intermediate service. Default: api.honeycomb.io"
                )
                config2
    in  config3

setupHoneycombAction :: Context τ -> IO Forwarder
setupHoneycombAction context = do
    let params = commandLineFrom context
        pairs = environmentValuesFrom params
        possibleTeam = lookupKeyValue "HONEYCOMB_TEAM" pairs
        possibleHoneycombHostOverride = lookupKeyValue "HONEYCOMB_HOST" pairs

    let defaultHoneycombHost = "api.honeycomb.io"
        honeycombHost = case possibleHoneycombHostOverride of
            -- Use the default
            Nothing -> defaultHoneycombHost
            Just Empty -> defaultHoneycombHost
            Just (Value "") -> defaultHoneycombHost
            -- Use the override
            Just (Value host) -> C.pack host

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

    r <- newIORef Nothing

    pure
        Forwarder
            { telemetryHandlerFrom = process r honeycombHost apikey dataset
            }

-- use partually applied
process :: IORef (Maybe Connection) -> Hostname -> ApiKey -> Dataset -> [Datum] -> IO ()
process r honeycombHost apikey dataset datums = do
    let targets = List.foldl' f emptyMap datums :: Map Dataset [JsonValue]
    let pairs = fromMap targets :: [(Dataset, [JsonValue])]

    forM_ pairs $ \(dataset', values') -> do
        let json = JsonArray values'
        postEventToHoneycombAPI r honeycombHost apikey dataset' json
  where
    f :: Map Dataset [JsonValue] -> Datum -> Map Dataset [JsonValue]
    f acc datum =
        let
            (override, point) = convertDatumToJson datum

            dataset' = case override of
                Nothing -> dataset
                Just value -> value

            list' = case lookupKeyValue dataset' acc of
                Nothing -> point : []
                Just list -> point : list
        in
            insertKeyValue dataset' list' acc

-- implements the spec described at <https://docs.honeycomb.io/getting-data-in/tracing/send-trace-data/>
convertDatumToJson :: Datum -> (Maybe Dataset, JsonValue)
convertDatumToJson datum =
    let spani = spanIdentifierFrom datum
        trace = traceIdentifierFrom datum
        parent = parentIdentifierFrom datum
        meta0 = attachedMetadataFrom datum

        meta1 = insertKeyValue "name" (JsonString (spanNameFrom datum)) meta0

        meta2 = case spani of
            Nothing -> case trace of
                Nothing -> meta1
                Just _ -> insertKeyValue "meta.annotation_type" (JsonString "span_event") meta1
            Just value -> insertKeyValue "trace.span_id" (JsonString (unSpan value)) meta1

        meta3 = case parent of
            Nothing -> case trace of
                Nothing -> meta2
                Just _ -> insertKeyValue "meta.span_type" (JsonString "root") meta2
            Just value -> insertKeyValue "trace.parent_id" (JsonString (unSpan value)) meta2

        meta4 = case trace of
            Nothing -> meta3
            Just value -> insertKeyValue "trace.trace_id" (JsonString (unTrace value)) meta3

        meta5 = case serviceNameFrom datum of
            Nothing -> meta4
            Just service -> insertKeyValue "service.name" (JsonString service) meta4

        meta6 = case durationFrom datum of
            Nothing -> meta5
            Just duration ->
                insertKeyValue
                    "duration_ms"
                    (JsonNumber (fromRational (toRational duration / 1e6)))
                    meta5

        time = intoRope (show (spanTimeFrom datum))
        point =
            JsonObject
                ( intoMap
                    [ (JsonKey "time", JsonString time)
                    , (JsonKey "data", JsonObject meta6)
                    ]
                )

        override = datasetFrom datum
    in  (override, point)

acquireConnection :: IORef (Maybe Connection) -> Hostname -> IO Connection
acquireConnection r honeycombHost = do
    possible <- readIORef r
    case possible of
        Nothing -> do
            ctx <- baselineContextSSL
            c <- openConnectionSSL ctx honeycombHost 443

            writeIORef r (Just c)
            pure c
        Just c -> do
            pure c

cleanupConnection :: IORef (Maybe Connection) -> IO ()
cleanupConnection r = do
    Safe.finally
        ( do
            possible <- readIORef r
            case possible of
                Nothing -> pure ()
                Just c -> closeConnection c
        )
        ( do
            writeIORef r Nothing
        )

compressBody :: Bytes -> OutputStream Builder -> IO ()
compressBody bytes o = do
    let x = fromBytes bytes
    let x' = GZip.compress x
    let b = Builder.lazyByteString x'
    Streams.write (Just b) o

postEventToHoneycombAPI :: IORef (Maybe Connection) -> Hostname -> ApiKey -> Dataset -> JsonValue -> IO ()
postEventToHoneycombAPI r honeycombHost apikey dataset json = attempt False
  where
    attempt retrying = do
        Safe.catch
            ( do
                c <- acquireConnection r honeycombHost

                -- actually transmit telemetry to Honeycomb
                sendRequest c q (compressBody (encodeToUTF8 json))
                receiveResponse c handler
            )
            ( \(e :: SomeException) -> do
                -- ideally we don't get here, but if the SSL connection collapses
                -- we will. We retry /once/, and otherwise throw the exception out.
                cleanupConnection r
                case retrying of
                    False -> do
                        putStrLn "internal: Reconnecting to Honeycomb"
                        attempt True
                    True -> do
                        putStrLn "internal: Failed to re-establish connection to Honeycomb"
                        Safe.throw e
            )

    q = buildRequest1 $ do
        http POST (fromRope ("/1/batch/" <> dataset))
        setContentType "application/json"
        setHeader "Content-Encoding" "gzip"
        setHeader "X-Honeycomb-Team" (fromRope (apikey))

    {-
    Response to Batch API looks like:

    [{"status":202}]

    TODO we need to handle other status responses properly.
    -}
    handler :: Response -> InputStream ByteString -> IO ()
    handler p i = do
        let code = getStatusCode p
        case code of
            200 -> do
                body <- simpleHandler p i
                let responses = decodeFromUTF8 (intoBytes body)
                case responses of
                    Just (JsonArray pairs) -> mapM_ f pairs
                      where
                        f pair = case pair of
                            JsonObject kvs -> case lookupKeyValue "status" kvs of
                                Just (JsonNumber 202) -> do
                                    -- normal response
                                    pure ()
                                _ -> do
                                    -- some other status!
                                    putStrLn "internal: Unexpected status returned;"
                                    C.putStrLn body
                            _ -> putStrLn "internal: wtf?"
                    _ -> do
                        putStrLn "internal: Unexpected response from Honeycomb"
                        C.putStrLn body
            _ -> do
                putStrLn "internal: Failed to post to Honeycomb"
                debugHandler p i
