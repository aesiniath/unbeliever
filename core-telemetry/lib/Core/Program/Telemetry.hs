{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)

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

instance Telemetry Int32 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Int64 where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromIntegral v))

instance Telemetry Integer where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromInteger v))

-- is this the efficient way to get to a Scientific?
instance Telemetry Float where
    metric k v = MetricValue (JsonKey k) (JsonNumber (fromRational (toRational v)))

-- is this the efficient way to get to a Scientific?
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
