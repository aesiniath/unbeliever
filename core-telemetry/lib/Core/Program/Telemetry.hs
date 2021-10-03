{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Program.Telemetry (
    MetricValue,
    Telemetry (metric),
    service,
    Exporter,
    initializeTelemetry,
    honeycomb,
    beginTrace,
    usingTrace,
    encloseSpan,
    telemetry,
    sendEvent,
) where

import Core.Encoding.Json
import Core.Program.Context
import Core.Text.Rope
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)

{- |
A telemetry value that can be sent over the wire. This is a wrapper around
Json values of type string, number, or boolean.
-}

-- a bit specific to Honeycomb's very limited data model, but what else is
-- there?
data MetricValue = MetricValue JsonKey JsonValue

{- |
Record the name of the service that this span and its children are a part of.
This will end up as the @service_name@ parameter when exported.
-}

-- This field name appears to be very Honeycomb specific, but looking around
-- Open Telemmtry it was just a property floating around and regardless of
-- what it gets called it needs to get sent.
service :: Rope -> MetricValue
service v = MetricValue "service_name" (JsonString v)

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

initializeTelemetry :: Exporter -> Context τ -> IO (Context τ)
initializeTelemetry exporter context =
    pure
        ( context
            { loggerExporterFrom = exporter
            }
        )

honeycomb :: Exporter
honeycomb = undefined

encloseSpan :: Rope -> Program z a -> Program z a
encloseSpan label action = undefined

{- do
    bracket
        (beginSpan)
        (endSpan)
        (\_ -> action)
    where
        beginSpan :: Program z Span
        beginSpan = undefined

        endSpan :: Program z ()
        ensSpan = do
            eventT
-}

beginTrace :: Program τ α -> Program τ α
beginTrace = undefined

usingTrace :: Rope -> Program τ α -> Program τ α
usingTrace = undefined

telemetry :: [MetricValue] -> Program τ ()
telemetry = undefined

sendEvent :: Rope -> [MetricValue] -> Program τ ()
sendEvent label = undefined
