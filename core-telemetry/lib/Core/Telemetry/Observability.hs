{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

{- |
To use this capability, first you need to initialize the telemetry subsystem
with an appropriate exporter.

@
import "Core.Program"
import "Core.Telemetry"

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' \"1.0\" 'Core.Program.Execute.None' ('simpleConfig' [])
    context' <- 'initializeTelemetry' ['Core.Telemetry.Console.consoleExporter', 'Core.Telemetry.Console.structuredExporter', 'Core.Telemetry.Console.honeycombExporter'] context
    'Core.Program.Execute.executeWith' context' program
@

/Traces and Spans/

For spans to be connected together by an observability tool they need to be
part of a /trace/.

At the top of your program or request loop you need to start a new trace (with
'beginTrace') or continue one inherited from another service (with
'usingTrace'):

@
program :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' ()
program = do
    'beginTrace' $ do
        'encloseSpan' \"Service Request\" $ do

            -- do stuff!
            
            ...

            -- add appropriate telemetry values to the span 
            'telemetry'
                [ 'metric' \"colour\" currentSkyObservation
                , 'metric' \"temperature" currentAirTemperature
                ]
@

will result in @colour=\"Blue\"@ and @temperature=26.1@ or whatever being sent
by the telemetry system to the observability service that's been activated.

The real magic here is that spans /nest/. As you go into each subcomponent on
your request path you can again call 'encloseSpan' creating a new span. Any
metrics added before entering the new span will be inherited by the subspan
and sent when it finishes so you don't have to keep re-attaching data if it's
common across all the spans in your trace.

/Events/

In other circumstances you will just want to send metrics:

@
            -- not again!
            'sendEvent' \"Cat meowed\"
                [ 'metric' \"room\" (\"living room\" :: 'Rope')
                , 'metric' "volume\" (127.44 :: 'Float') -- decibels
                , 'metric' \"apparently_hungry\" 'True'
                ]
@

will result in @room=\"living room\"@, @volume=127.44@, and
@apparently_hungry=true@ being sent as you'd expect. Ordinarily when you call
'metric' you are passing in a variable that already has a type, but when
hardcoding literals like in this example (less common but not unheard of)
you'll need to add a type annotation.

You /do not/ have to call 'sendEvent' from within a span, but if you do
appropriate metadata will be added to help the observability system link the
event to the context of the span it occured during.

Either way, explicitly sending an event, or upon exiting a span, the telemetry
will be gathered up and sent via the chosen exporter and forwarded to the
observability or monitoring service you have chosen.
-}
module Core.Telemetry.Observability (
    -- * Initializing
    Exporter,
    initializeTelemetry,

    -- * Traces
    Trace (..),
    Span (..),
    beginTrace,
    usingTrace,
    setServiceName,

    -- * Spans
    Label,
    encloseSpan,
    setStartTime,

    -- * Creating telemetry
    MetricValue,
    Telemetry (metric),
    telemetry,

    -- * Events
    sendEvent,
) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Core.Data.Structures (Map, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (liftIO)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Rope
import Core.Text.Utilities (oxford, quote)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Char (chr)
import Data.Int (Int32, Int64)
import qualified Data.List as List (foldl')
import Data.Scientific (Scientific)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)
import System.Random (newStdGen, randomRs)

{- |
A telemetry value that can be sent over the wire. This is a wrapper around
JSON values of type string, number, or boolean. You create these using the
'metric' method provided by a 'Telemetry' instance and passing them to the
'telemetry' function in a span or 'sendEvent' if noting an event.
-}

-- a bit specific to Honeycomb's very limited data model, but what else is
-- there?
data MetricValue
    = MetricValue JsonKey JsonValue
    deriving (Show)

{- |
Record the name of the service that this span and its children are a part of.
A reasonable default is the name of the binary that's running, but frequently
you'll want to put something a bit more nuanced or specific to your
application. This is the overall name of the independent service, component,
or program complimenting the @label@ set when calling 'encloseSpan', which by
contrast descibes the name of the current phase, step, or even function name
within the overall scope of the \"service\".

This will end up as the @service_name@ parameter when exported.
-}

-- This field name appears to be very Honeycomb specific, but looking around
-- Open Telemmtry it was just a property floating around and regardless of
-- what it gets called it needs to get sent.
setServiceName :: Rope -> Program τ ()
setServiceName service = do
    context <- getContext
    let v = currentDatumFrom context
    liftIO $ do
        modifyMVar_
            v
            ( \datum -> do
                let datum' =
                        datum
                            { serviceNameFrom = Just service
                            }
                pure datum'
            )

class Telemetry σ where
    metric :: Rope -> σ -> MetricValue

instance Telemetry Int where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Int32 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Int64 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Integer where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromInteger v))

-- HELP is this the efficient way to get to a Scientific?
instance Telemetry Float where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromRational (toRational v)))

-- HELP is this the efficient way to get to a Scientific?
instance Telemetry Double where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromRational (toRational v)))

instance Telemetry Scientific where
    metric k v = MetricValue (JsonKey k) (JsonNumber v)

instance Telemetry Rope where
    metric k v = MetricValue (JsonKey k) (JsonString v)

instance Telemetry String where
    metric k v = MetricValue (JsonKey k) (JsonString (intoRope v))

{- |
The usual warning about assuming the @ByteString@ is ASCII or UTF-8 applies
here. Don't use this to send binary mush.
-}
instance Telemetry B.ByteString where
    metric k v = MetricValue (JsonKey k) (JsonString (intoRope v))

{- |
The usual warning about assuming the @ByteString@ is ASCII or UTF-8 applies
here. Don't use this to send binary mush.
-}
instance Telemetry L.ByteString where
    metric k v = MetricValue (JsonKey k) (JsonString (intoRope v))

instance Telemetry T.Text where
    metric k v = MetricValue (JsonKey k) (JsonString (intoRope v))

instance Telemetry U.Text where
    metric k v = MetricValue (JsonKey k) (JsonString (intoRope v))

instance Telemetry Bool where
    metric k v = MetricValue (JsonKey k) (JsonBool v)

instance Telemetry JsonValue where
    metric k v = MetricValue (JsonKey k) v

{- |
Activate the telemetry subsystem for use within the
'Core.Program.Execute.Program' monad.

Each exporter specified here will add setup and configuration to the context,
including command-line options and environment variables needed as
approrpiate:

@
    context' <- 'initializeTelemetry' ['Core.Telemetry.Console.consoleExporter'] context
@

This will allow you to then select the appropriate backend at runtime:

@
$ burger-service --telemetry=console
@

which will result in it spitting out metrics as it goes,

@
  calories = 667.0
  flavour = true
  meal_name = "hamburger"
  precise = 45.0
@

and so on.
-}
initializeTelemetry :: [Exporter] -> Context τ -> IO (Context τ)
initializeTelemetry exporters1 context =
    let exporters0 = initialExportersFrom context
        exporters2 = exporters0 ++ exporters1

        codenames =
            fmap (\name -> singletonRope '"' <> name <> singletonRope '"')
                . fmap codenameFrom
                $ exporters2

        config0 = initialConfigFrom context
        config1 =
            appendOption
                ( Option
                    "telemetry"
                    Nothing
                    (Value "EXPORTER")
                    ( [quote|
                    Turn on telemetry. Tracing data and metrics from events
                    will be forwarded via the specified exporter. Valid values
                    are
                      |]
                        <> oxford codenames
                    )
                )
                config0

        config2 = List.foldl' f config1 exporters2
     in pure
            ( context
                { initialConfigFrom = config2
                , initialExportersFrom = exporters2
                }
            )
  where
    -- This doesn't actually setup the telemetry processor; that's done in
    -- executeAction. Here we're setting up each  of the exporters so they
    -- show up in --help. When we process command-line arguments we'll find
    -- out which exporter was activated, if any.
    f :: Config -> Exporter -> Config
    f config exporter =
        let setup = setupConfigFrom exporter
         in setup config

type Label = Rope

{- |
Begin a span.

You need to call this from within the context of a trace, which is established
either by calling `beginTrace` or `usingTrace` somewhere above this point in
the program.

You can nest spans as you make your way through your program, which means each
span has a parent (except for the first one, which is the root span) In the
context of a trace, allows an observability tool to reconstruct the sequence
of events and to display them as a nested tree correspoding to your program
flow.

The current time will be noted when entering the 'Program' this span encloses,
and its duration recorded when the sub @Program@ exits. Start time, duration,
the unique identifier of the span (generated for you), the identifier of the
parent, and the unique identifier of the overall trace will be appended as
metadata points and then sent to the telemetry channel.
-}
encloseSpan :: Label -> Program z a -> Program z a
encloseSpan label action = do
    context <- getContext

    unique <- liftIO randomIdentifier
    debug "span" unique

    liftIO $ do
        -- prepare new span
        start <- getCurrentTimeNanoseconds

        -- slightly tricky: create a new Context with a new MVar with an
        -- forked copy of the current Datum, creating the nested span.
        let v = currentDatumFrom context
        datum <- readMVar v

        let datum' =
                datum
                    { spanIdentifierFrom = Just (Span unique)
                    , spanNameFrom = label
                    , spanTimeFrom = start
                    , parentIdentifierFrom = spanIdentifierFrom datum
                    }

        v2 <- newMVar datum'

        let context2 =
                context
                    { currentDatumFrom = v2
                    }

        -- execute nested program

        result <- subProgram context2 action

        -- extract the Datum as it stands after running the action, finalize
        -- with its duration, and send it
        finish <- getCurrentTimeNanoseconds
        datum2 <- readMVar v2
        let datum2' =
                datum2
                    { durationFrom = Just (unTimeStamp finish - unTimeStamp start)
                    }

        let tel = telemetryChannelFrom context

        atomically $ do
            writeTQueue tel (Just datum2')

        -- now back to your regularly scheduled Haskell program
        pure result

represent :: Int -> Char
represent x
    | x < 10 = chr (48 + x)
    | x < 36 = chr (65 + x - 10)
    | x < 62 = chr (97 + x - 36)
    | otherwise = '@'

-- TODO replace this with something that gets a UUID
randomIdentifier :: IO Rope
randomIdentifier = do
    gen <- newStdGen
    let result = packRope . fmap represent . take 16 . randomRs (0, 61) $ gen
    pure result

{- |
Start a new trace. A random identifier will be generated.

You /must/ have a single \"root span\" immediately below starting a new trace.

@
program :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' ()
program = do
    'beginTrace' $ do
        'encloseSpan' \"Service Request\" $ do
            ...
@
-}
beginTrace :: Program τ α -> Program τ α
beginTrace action = do
    trace <- liftIO randomIdentifier
    usingTrace (Trace trace) Nothing action

{- |
Begin a new trace, but using a trace identifier provided externally. This is
the most common case. Internal services that are play a part of a larger
request will inherit a job identifier, sequence number, or other externally
supplied unique code. Even an internet facing web service might have a
correlation ID provided by the outside load balancers.

If you are continuting an existing trace within the execution path of another,
larger, enclosing service then you need to specify what the parent span's
identifier is in the second argument.

@
program :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' ()
program = do
    
    -- do something that gets the trace ID
    trace <- ...

    -- and somethign to get the parent span ID
    parent <- ...

    'usingTrace' ('Trace' trace) ('Just' ('Span' span)) $ do
        'encloseSpan' \"Internal processing\" $ do
            ...
@
-}
usingTrace :: Trace -> Maybe Span -> Program τ α -> Program τ α
usingTrace trace possibleParent action = do
    context <- getContext

    case possibleParent of
        Nothing -> do
            debug "trace" (unTrace trace)
        Just parent -> do
            debug "trace" (unTrace trace)
            debug "parent" (unSpan parent)

    liftIO $ do
        -- prepare new span
        let v = currentDatumFrom context
        datum <- readMVar v

        let datum2 =
                datum
                    { traceIdentifierFrom = Just trace
                    , parentIdentifierFrom = possibleParent
                    }

        -- fork the Context
        v2 <- newMVar datum2

        let context2 =
                context
                    { currentDatumFrom = v2
                    }

        -- execute nested program
        subProgram context2 action

{- |
Add measurements to the current span.

@
            telemetry
                [ metric "calories" (667 :: Int)
                , metric "precise" measurement
                , metric "meal_name" ("hamburger" :: Rope)
                , metric "flavour" True
                ]
@

The 'metric' function is a method provided by instances of the 'Telemtetry'
typeclass which is mostly a wrapper around constructing key/value pairs
suitable to be sent as measurements up to an observability service.
-}
telemetry :: [MetricValue] -> Program τ ()
telemetry values = do
    context <- getContext

    liftIO $ do
        -- get the map out
        let v = currentDatumFrom context
        modifyMVar_
            v
            ( \datum -> do
                let meta = attachedMetadataFrom datum

                -- update the map
                let meta' = List.foldl' f meta values

                -- replace the map back into the Datum (and thereby back into the
                -- Context), updating it
                let datum' =
                        datum
                            { attachedMetadataFrom = meta'
                            }
                pure datum'
            )
  where
    f :: Map JsonKey JsonValue -> MetricValue -> Map JsonKey JsonValue
    f acc (MetricValue k v) = insertKeyValue k v acc

{- |
Record telemetry about an event. Specify a label for the event and then
whichever metrics you wish to record.

The emphasis of this package is to create traces and spans. There are,
however, times when you just want to send telemetry about an event. You can
use 'sendEvent' to accomplush this.

If you call 'sendEvent' within an enclosing span created with 'encloseSpan'
(the usual and expected use case) then this event will be \"linked\" to this
span so that the observability tool can deisplay it attached to the span in
the in which it occured.

Not every situation is in the context of traces and spans and so you can use
this to send arbitrary telemetry.
-}
sendEvent :: Label -> [MetricValue] -> Program τ ()
sendEvent label values = do
    context <- getContext

    liftIO $ do
        now <- getCurrentTimeNanoseconds
        -- get the map out
        let v = currentDatumFrom context
        datum <- readMVar v

        let meta = attachedMetadataFrom datum

        -- update the map
        let meta' = List.foldl' f meta values
        -- replace the map back into the Datum and queue for sending
        let datum' =
                datum
                    { spanNameFrom = label
                    , spanIdentifierFrom = Nothing
                    , parentIdentifierFrom = spanIdentifierFrom datum
                    , spanTimeFrom = now
                    , attachedMetadataFrom = meta'
                    }

        let tel = telemetryChannelFrom context
        atomically $ do
            writeTQueue tel (Just datum')
  where
    f :: Map JsonKey JsonValue -> MetricValue -> Map JsonKey JsonValue
    f acc (MetricValue k v) = insertKeyValue k v acc

-- get current time after digging out datum and override spanTimeFrom before
-- sending Datum

{- |
Override the start time of the current span.

Under normal circumstances this shouldn't be necessary. The start and end of a
span are recorded automatically when calling 'encloseSpan'. Observabilty tools
are designed to be used live; traces and spans should be created in real time
in your code.
-}
setStartTime :: TimeStamp -> Program τ ()
setStartTime time = do
    context <- getContext

    liftIO $ do
        -- get the map out
        let v = currentDatumFrom context
        modifyMVar_
            v
            (\datum -> pure datum{spanTimeFrom = time})
