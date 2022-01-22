{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
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

    -- * Internals
    generateIdentifierTrace,
    generateIdentifierSpan,
    convertToTrace64,
    convertToSpan32,
    toHexNormal64,
    toHexReversed64,
    toHexNormal32,
    toHexReversed32,
    knownMachineIdentity,
) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Core.Data.Structures (Map, emptyMap, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Logging
import Core.System (unsafePerformIO)
import Core.System.Base (liftIO)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Rope
import Core.Text.Utilities (oxford, quote)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.List as List (foldl')
import Data.Scientific (Scientific)
import qualified Data.Text as T (Text)
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import qualified Data.Text.Lazy as U (Text)
import GHC.Int
import GHC.Word
import Network.Info (MAC (..), NetworkInterface, getNetworkInterfaces, mac)
import System.Random (randomRIO)

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

    start <- liftIO $ do
        getCurrentTimeNanoseconds

    unique <- generateIdentifierSpan start

    internal label emptyRope
    internal "span = " (unSpan unique)

    liftIO $ do
        -- prepare new span

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

{-
Get the MAC address of the first interface that's not the loopback device. If
something goes weird then we return a valid but bogus address (in the locally
administered addresses block).
-}
knownMachineIdentity :: MAC
knownMachineIdentity = unsafePerformIO $ do
    interfaces <- getNetworkInterfaces
    pure (go interfaces)
  where
    go :: [NetworkInterface] -> MAC
    go [] = bogusAddress
    go (interface : remainder) =
        let address = mac interface
         in if address /= loopbackAddress
                then address
                else go remainder

    loopbackAddress = MAC 00 00 00 00 00 00
    bogusAddress = MAC 0xfe 0xff 0xff 0xff 0xff 0xff
{-# NOINLINE knownMachineIdentity #-}

{- |
Generate an identifier suitable for use in a trace context. Trace identifiers
are 16 bytes. We incorporate the time to nanosecond precision, the host
system's MAC address, and a random element. This is similar to a version 1
UUID, but we render the least significant bits of the time stamp ordered first
so that visual distinctiveness is on the left. The MAC address in the lower 48
bits is /not/ reversed, leaving the most distinctiveness [the actual host as
opposed to manufacturer OIN] hanging on the right hand edge of the identifier.
The two bytes of randomness are in the middle.
-}
generateIdentifierTrace :: Program τ Trace
generateIdentifierTrace = do
    now <-
        liftIO $
            getCurrentTimeNanoseconds
    rand <-
        liftIO $
            randomRIO (0, 65535)

    pure
        ( Trace
            (convertToTrace64 now rand knownMachineIdentity)
        )

convertToTrace64 :: TimeStamp -> Word16 -> MAC -> Rope
convertToTrace64 time rand address =
    let p1 = packRope (toHexReversed64 (fromIntegral time))
        p2 = packRope (toHexNormal16 rand)
        p3 = packRope (convertMACToHex address)
     in p1 <> p2 <> p3

convertMACToHex :: MAC -> [Char]
convertMACToHex (MAC b1 b2 b3 b4 b5 b6) =
    nibbleToHex b1 4 :
    nibbleToHex b1 0 :
    nibbleToHex b2 4 :
    nibbleToHex b2 0 :
    nibbleToHex b3 4 :
    nibbleToHex b3 0 :
    nibbleToHex b4 4 :
    nibbleToHex b4 0 :
    nibbleToHex b5 4 :
    nibbleToHex b5 0 :
    nibbleToHex b6 4 :
    nibbleToHex b6 0 :
    []
  where
    nibbleToHex w = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexReversed64 :: Word64 -> [Char]
toHexReversed64 w =
    nibbleToHex 00 :
    nibbleToHex 04 :
    nibbleToHex 08 :
    nibbleToHex 12 :
    nibbleToHex 16 :
    nibbleToHex 20 :
    nibbleToHex 24 :
    nibbleToHex 28 : -- Word32
    nibbleToHex 32 :
    nibbleToHex 36 :
    nibbleToHex 40 :
    nibbleToHex 44 :
    nibbleToHex 48 :
    nibbleToHex 52 :
    nibbleToHex 56 :
    nibbleToHex 60 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal64 :: Word64 -> [Char]
toHexNormal64 w =
    nibbleToHex 60 :
    nibbleToHex 56 :
    nibbleToHex 52 :
    nibbleToHex 48 :
    nibbleToHex 44 :
    nibbleToHex 40 :
    nibbleToHex 36 :
    nibbleToHex 32 :
    nibbleToHex 28 : -- Word32
    nibbleToHex 24 :
    nibbleToHex 20 :
    nibbleToHex 16 :
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

--
-- Convert a 32-bit word to eight characters, but reversed so the least
-- significant bits are first.
--
toHexReversed32 :: Word32 -> [Char]
toHexReversed32 w =
    nibbleToHex 00 :
    nibbleToHex 04 :
    nibbleToHex 08 :
    nibbleToHex 12 :
    nibbleToHex 16 :
    nibbleToHex 20 :
    nibbleToHex 24 :
    nibbleToHex 28 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal32 :: Word32 -> [Char]
toHexNormal32 w =
    nibbleToHex 28 :
    nibbleToHex 24 :
    nibbleToHex 20 :
    nibbleToHex 16 :
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal16 :: Word16 -> [Char]
toHexNormal16 w =
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

{-
byteToHex :: Word8 -> [Char]
byteToHex c =
    let !low = unsafeToDigit (c .&. 0x0f)
        !hi = unsafeToDigit ((c .&. 0xf0) `shiftR` 4)
     in hi : low : []
-}

-- convert a nibble to its hexidecimal character equivalent
unsafeToDigit :: Word8 -> Char
unsafeToDigit w =
    if w < 10
        then unsafeChr8 (48 + w)
        else unsafeChr8 (97 + w - 10)

{- |
Generate an identifier for a span. We only have 8 bytes to work with. We use the
nanosecond prescision timestamp with the nibbles reversed.
-}
generateIdentifierSpan :: TimeStamp -> Program τ Span
generateIdentifierSpan t = do
    pure
        ( Span
            (convertToSpan32 t)
        )

convertToSpan32 :: TimeStamp -> Rope
convertToSpan32 t =
    let w = fromIntegral (unTimeStamp t) :: Word64
     in packRope (toHexReversed64 w)

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
    trace <- generateIdentifierTrace
    usingTrace trace Nothing action

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

    -- and something to get the parent span ID
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
            internal "trace = " (unTrace trace)
        Just parent -> do
            internal "trace = " (unTrace trace)
            internal "parent = " (unSpan parent)

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
        'telemetry'
            [ 'metric' \"calories\" (667 :: 'Int')
            , 'metric' \"precise\" measurement
            , 'metric' \"meal_name\" ("hamburger" :: 'Rope')
            , 'metric' \"flavour\" 'True'
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
