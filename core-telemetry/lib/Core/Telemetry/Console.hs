{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
A simple exporter backend that prints your metrics to the terminal as they are
submitted.

Taking the example from 'Core.Telemetry.Observability.telemetry', the output
would be:

@
09:58:54Z (03.755) Process order:
  calories = 667.0
  flavour = true
  meal_name = "hamburger"
  precise = 45.0
@
-}
module Core.Telemetry.Console (
    consoleExporter,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Core.Data.Structures (fromMap)
import Core.Encoding.Json
import Core.Program.Arguments
import Core.Program.Context
import Core.Program.Logging
import Core.System.External (getCurrentTimeNanoseconds)
import Core.Text.Colour
import Core.Text.Rope
import Core.Text.Utilities
import qualified Data.List as List

{-|
Output metrics to the terminal. This is mostly useful for debugging, but it
can also be used as general output mechanism if your program is mostly
concerned with gathering metrics and displaying them.
-}
consoleExporter :: Exporter
consoleExporter =
    Exporter
        { codenameFrom = "console"
        , setupConfigFrom = setupConsoleConfig
        , setupActionFrom = setupConsoleAction
        }

setupConsoleConfig :: Config -> Config
setupConsoleConfig = id

setupConsoleAction :: Context τ -> IO Forwarder
setupConsoleAction context = do
    let out = outputChannelFrom context
    pure
        ( Forwarder
            { telemetryHandlerFrom = processConsoleOutput out
            }
        )

processConsoleOutput :: TQueue (Maybe Rope) -> [Datum] -> IO ()
processConsoleOutput out datums = do
    mapM_ processOne datums
  where
    processOne :: Datum -> IO ()
    processOne datum = do
        let start = spanTimeFrom datum
        let text =
                (intoEscapes pureGrey)
                    <> spanNameFrom datum
                    <> singletonRope ':'
                    <> let pairs :: [(JsonKey, JsonValue)]
                           pairs = fromMap (attachedMetadataFrom datum)
                        in List.foldl' f emptyRope pairs
                            <> (intoEscapes resetColour)

        now <- getCurrentTimeNanoseconds
        let result =
                formatLogMessage
                    start
                    now
                    SeverityDebug
                    text
        atomically $ do
            writeTQueue out (Just result)

f :: Rope -> (JsonKey, JsonValue) -> Rope
f acc (k, v) =
    acc <> "\n  "
        <> (intoEscapes pureGrey)
        <> intoRope k
        <> " = "
        <> render 80 v
