{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Core.Program.Telemetry (
    eventT,
    debugT,
    MetricValue,
    Telemetry (metric),
    Exporter,
    initializeTelemetry,
    honeycomb,
    beginTrace,
    usingTrace,
    encloseSpan,
    telemetry,
) where

import Core.Encoding.Json
import Core.Program.Execute
import Core.Text.Rope

eventT :: Telemetry σ => σ -> Program τ ()
eventT = undefined

debugT :: Telemetry σ => Rope -> σ -> Program τ ()
debugT = undefined

{- |
A telemetry value that can be sent over the wire. This is a wrapper around
Json values of type string, number, or boolean.
-}

-- a bit specific to Honeycomb's very limited data model, but what else is
-- there?
data MetricValue = MetricValue JsonKey JsonValue

class Telemetry σ where
    metric :: Rope -> σ -> MetricValue

instance Telemetry Int where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Float where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromRational (toRational v)))

instance Telemetry Double where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromRational (toRational v)))

instance Telemetry Rope where
    metric k v = MetricValue (JsonKey k) (JsonString v)

instance Telemetry Bool where
    metric k v = MetricValue (JsonKey k) (JsonBool v)

initializeTelemetry :: Exporter -> Context τ -> IO (Context τ)
initializeTelemetry = undefined

data Exporter = Exporter

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
