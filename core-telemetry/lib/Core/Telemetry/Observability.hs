{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Traditional \"monitoring\" systems were concerned with gathering together obscene
quantities of metrics and graphing them. This makes for /very/ pretty billboard
displays in Network Operations Centers which impress visitors tremendously,
but (it turns out) are of limited use when actually trying to troubleshoot
problems or improve the performance of our systems.  We all put a lot of
effort into trying to detect anamolies but really, despite person-centuries of
effort, graphing raw system metrics doesn't get us as far as we would have liked.

Experience with large-scale distributed systems has led to the insight that
what you need is to be able to trace the path a request takes as it moves
through a system, correlating and comparing this trace to others like it. This
has led to the modern \"observability\" movement, more concerned with metrics
which descirbe user-visible experience, service levels, error budgets, and
being able to do ad-hoc analysis of evolving situations.

This library aims to support both models of using telemetry, with the primary
emphasis being on the /traces/ and /spans/ that can be connected together by
an observability tool.

= Usage

To use this capability, first you need to initialize the telemetry subsystem
with an appropriate exporter:

@
import "Core.Program"
import "Core.Telemetry"

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' \"1.0\" 'Core.Program.Execute.None' ('simpleConfig' [])
    context' <- 'initializeTelemetry' ['Core.Telemetry.Console.consoleExporter', 'Core.Telemetry.Structured.structuredExporter', 'Core.Telemetry.Honeycomb.honeycombExporter'] context
    'Core.Program.Execute.executeWith' context' program
@

Then when you run your program you can pick the exporter:

@
\$ __burgerservice --telemetry=structured__
@

to activate sending telemetry, in this case, to the console in the form of
structured JSON logs. Other exporters add additional command-line options with
which to configure how and where the metrics will be sent.

= Traces and Spans

At the top of your program or request loop you need to start a new trace (with
'beginTrace') or continue one inherited from another service (with
'usingTrace'):

@
program :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' ()
program = do
    'beginTrace' $ do
        'encloseSpan' \"Service request\" $ do

            -- do stuff!

            ...

            obs <- currentSkyObservation
            temp <- currentAirTemperature

            ...

            -- add appropriate telemetry values to the span
            'telemetry'
                [ 'metric' \"sky_colour\" (colourFrom obs)
                , 'metric' \"temperature" temp
                ]
@

will result in @sky_colour=\"Blue\"@ and @temperature=26.1@ or whatever being
sent by the telemetry system to the observability service that's been
activated.

The real magic here is that spans /nest/. As you go into each subcomponent on
your request path you can again call 'encloseSpan' creating a new span, which
can have its own telemetry:

@
currentSkyObservation :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' Observation
currentSkyObservation = do
    'encloseSpan' "Observe sky" $ do
        ...

        'telemetry'
            [ 'metric' \"radar_frequency\" freq
            , 'metric' \"cloud_cover\" blockageLevel
            ]

        'pure' result
@

Any metrics added before entering the new span will be inherited by the
subspan and sent when it finishes so you don't have to keep re-attaching data
if it's common across all the spans in your trace.

= Events

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
    usingTrace',
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
    clearMetrics,
    clearTrace,
) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Safe qualified as Safe
import Core.Data.Clock
import Core.Data.Structures (Map, emptyMap, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (SomeException, liftIO)
import Core.Telemetry.Identifiers
import Core.Text.Rope
import Core.Text.Utilities (oxford, quote)
import Data.ByteString qualified as B (ByteString)
import Data.ByteString.Lazy qualified as L (ByteString)
import Data.List qualified as List (foldl')
import Data.Scientific (Scientific)
import Data.Text qualified as T (Text)
import Data.Text.Lazy qualified as U (Text)
import GHC.Int
import GHC.Word
import System.Random (randomIO)

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

This will end up as the @service.name@ parameter when exported.
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

{- |
Adaptor class to take primitive values and send them as metrics. The
underlying types are either strings, numbers, or boolean so any instance will
need to externalize and then convert to one of these three.

(this class is what allows us to act pass in what look like polymorphic lists
of metrics to 'telemetry' and 'sendEvent')
-}
class Telemetry σ where
    metric :: Rope -> σ -> MetricValue

instance Telemetry Int where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Int32 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Int64 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Word32 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Word64 where
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

instance Telemetry () where
    metric k _ = MetricValue (JsonKey k) JsonNull

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
\$ __burgerservice --telemetry=console__
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

    liftIO $ do
        -- prepare new span
        start <- getCurrentTimeNanoseconds

        rand <- randomIO :: IO Word16

        let unique = createIdentifierSpan start rand

        subProgram context $ do
            internal ("Enter " <> label)
            internal ("span = " <> unSpan unique)

        -- slightly tricky: create a new Context with a new MVar with an
        -- forked copy of the current Datum, creating the nested span.
        let v = currentDatumFrom context
        datum <- readMVar v

        let datum' =
                datum
                    { spanIdentifierFrom = Just unique
                    , spanNameFrom = label
                    , spanTimeFrom = start
                    , parentIdentifierFrom = spanIdentifierFrom datum
                    }

        v2 <- newMVar datum'

        let context2 =
                context
                    { currentDatumFrom = v2
                    }

        -- execute nested program. We have to use try (c.f. catch) so that if
        -- an exception has occurred we still enqueue the telemetry datum
        -- before bailing out.
        result :: Either SomeException a <-
            Safe.try
                (subProgram context2 action)

        subProgram context $ do
            internal ("Leave " <> label)

        -- extract the Datum as it stands after running the action, finalize
        -- with its duration, and send it. Note that we don't use the original
        -- start time as it may have been overwritten.
        finish <- getCurrentTimeNanoseconds
        datum2 <- readMVar v2
        let start2 = spanTimeFrom datum2
        let datum2' =
                datum2
                    { durationFrom = Just (unTime finish - unTime start2)
                    }

        let tel = telemetryChannelFrom context

        atomically $ do
            writeTQueue tel (Just datum2')

        -- now back to your regularly scheduled Haskell program
        case result of
            Left e -> Safe.throw e
            Right value -> pure value

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
    now <- liftIO $ do
        getCurrentTimeNanoseconds

    rand <- liftIO $ do
        (randomIO :: IO Word16)

    let trace = createIdentifierTrace now rand hostMachineIdentity

    internal "Begin trace"
    internal ("trace = " <> unTrace trace)

    encloseTrace trace Nothing action

{- |
Continue an existing trace using a 'Trace' identifier and parent 'Span'
identifier sourced externally. This is the most common case. Internal services
that play a part of a larger request will inherit a job identifier, sequence
number, or other externally supplied unique code. Even an internet-facing web
service might have a correlation ID provided by the outside load balancers.

@
program :: 'Core.Program.Execute.Program' 'Core.Program.Execute.None' ()
program = do

    -- do something that gets the trace ID
    trace <- ...

    -- and something to get the parent span ID
    parent <- ...

    'usingTrace' ('Trace' trace) ('Span' parent) $ do
        'encloseSpan' \"Internal processing\" $ do
            ...
@

@since 0.2.0
-}
usingTrace :: Trace -> Span -> Program τ α -> Program τ α
usingTrace trace parent action = do
    internal "Using trace"
    internal ("trace = " <> unTrace trace)
    internal ("parent = " <> unSpan parent)

    encloseTrace trace (Just parent) action

{- |
Create a new trace with the specified 'Trace' identifier. Unlike 'usingTrace'
this does /not/ set the parent 'Span' identifier, thereby marking this as a
new trace and causing the first span enclosed within this trace to be
considered the \"root\" span of the trace. This is unusual and should only
expected to be used in concert with the 'setIdentifierSpan' override to create
a root spans in asynchronous processes /after/ all the child spans have
already been composed and sent.

Most times, you don't need this. You're much better off using 'beginTrace' to
create a root span. However, life is not kind, and sometimes bad things happen
to good abstractions. Maybe you're tracing your build system, which isn't
obliging enough to be all contained in one Haskell process, but is a
half-dozen steps shotgunned across several different processes. In situations
like this, it's useful to be able to generate a 'Trace' identifier and 'Span'
identifier, use that as the parent across several different process
executions, hanging children spans off of this as you go, then manually send
up the root span at the end of it all.

@
    trace <- ...
    unique <- ...

    -- many child spans in other processes have used these as trace
    -- identifiers and parent span identifier. Now form the root span thereby
    -- finishing the trace.

    'usingTrace'' trace $ do
        'encloseSpan' \"Launch Missiles\" $ do
            'setStartTime' start
            'setIdentifierSpan' unique
            'telemetry'
                [ 'metric' ...
                ]
@

@since 0.2.1
-}
usingTrace' :: Trace -> Program τ α -> Program τ α
usingTrace' trace action = do
    internal "Using trace"
    internal ("trace = " <> unTrace trace)

    encloseTrace trace Nothing action

encloseTrace :: Trace -> Maybe Span -> Program τ α -> Program τ α
encloseTrace trace possibleParent action = do
    context <- getContext

    liftIO $ do
        -- prepare new span
        let v = currentDatumFrom context
        datum <- readMVar v

        let datum2 =
                datum
                    { traceIdentifierFrom = Just trace
                    , spanIdentifierFrom = possibleParent
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
        'telemetry'
            [ 'metric' \"calories\" (667 :: 'Int')
            , 'metric' \"precise\" measurement
            , 'metric' \"meal_name\" ("hamburger" :: 'Rope')
            , 'metric' \"flavour\" 'True'
            ]
@

The 'metric' function is a method provided by instances of the 'Telemetry'
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
    f acc (MetricValue k@(JsonKey text) v) =
        if nullRope text
            then error "Empty metric field name not allowed"
            else insertKeyValue k v acc

{- |
Record telemetry about an event. Specify a label for the event and then
whichever metrics you wish to record.

The emphasis of this package is to create traces and spans. There are,
however, times when you just want to send telemetry about an event. You can
use 'sendEvent' to accomplish this.

If you do call 'sendEvent' within an enclosing span created with 'encloseSpan'
(the usual and expected use case) then this event will be \"linked\" to this
span so that the observability tool can display it attached to the span in the
in which it occured.

@
        'sendEvent'
            "Make tea"
            [ 'metric' \"sugar\" 'False'
            ]
@
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
    f acc (MetricValue k@(JsonKey text) v) =
        if nullRope text
            then error "Empty metric field name not allowed"
            else insertKeyValue k v acc

-- get current time after digging out datum and override spanTimeFrom before
-- sending Datum

{- |
Override the start time of the current span.

Under normal circumstances this shouldn't be necessary. The start and end of a
span are recorded automatically when calling 'encloseSpan'. Observabilty tools
are designed to be used live; traces and spans should be created in real time
in your code.
-}
setStartTime :: Time -> Program τ ()
setStartTime time = do
    context <- getContext

    liftIO $ do
        -- get the map out
        let v = currentDatumFrom context
        modifyMVar_
            v
            (\datum -> pure datum{spanTimeFrom = time})

{- |
Reset the accumulated metadata metrics to the emtpy set.

This isn't something you'd need in normal circumstances, as inheriting
contextual metrics from surrounding code is usually what you want. But if you
have a significant change of setting then clearing the attached metadata may
be appropriate; after all, observability tools visualizing a trace will show
you the context an event was encountered in.
-}
clearMetrics :: Program τ ()
clearMetrics = do
    context <- getContext

    liftIO $ do
        -- get the map out
        let v = currentDatumFrom context
        modifyMVar_
            v
            (\datum -> pure datum{attachedMetadataFrom = emptyMap})

{- |
Reset the program context so that the currently executing program is no longer
within a trace or span.

This is specifically for the occasion where you have forked a new thread but
have not yet received the event which would occasion starting a new trace.

@since 0.2.4
-}
clearTrace :: Program τ ()
clearTrace = do
    context <- getContext

    liftIO $ do
        let v = currentDatumFrom context
        modifyMVar_
            v
            (\_ -> pure emptyDatum)
