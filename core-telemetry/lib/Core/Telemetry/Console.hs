{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Implementations of different backends that telemetry can be exported to.
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

-- TODO convert this into a Render instance

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
            { telemetryHandlerFrom = process out
            }
        )

process :: TQueue (Maybe Rope) -> Datum -> IO ()
process out datum = do
    now <- getCurrentTimeNanoseconds
    let start = spanTimeFrom datum
    let text =
            (intoEscapes pureGrey)
                <> spanNameFrom datum
                <> singletonRope ':'
                <> let pairs :: [(JsonKey, JsonValue)]
                       pairs = fromMap (attachedMetadataFrom datum)
                    in List.foldl' f emptyRope pairs
                        <> (intoEscapes resetColour)

    let result = formatLogMessage start now SeverityDebug text
    atomically $ do
        writeTQueue out (Just result)

f :: Rope -> (JsonKey, JsonValue) -> Rope
f acc (k, v) =
    acc <> "\n  "
        <> (intoEscapes pureGrey)
        <> intoRope k
        <> " = "
        <> render 80 v
