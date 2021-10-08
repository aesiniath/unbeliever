{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Implementations of different backends that telemetry can be exported to.
-}
module Core.Telemetry.Backends (
    Exporter,
    debugExporter,
    honeycomb,
) where

import Core.Data.Structures (Map, insertKeyValue)
import Core.Encoding.Json
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base (stdout)
import Core.System.External (TimeStamp (unTimeStamp), getCurrentTimeNanoseconds)
import Core.Text.Colour
import Core.Text.Rope
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)

debugExporter :: Exporter
debugExporter =
    Exporter
        { processorFrom = \datum ->
            let text =
                    (intoEscapes pureBlue)
                        <> datumNameFrom datum
                        <> (intoEscapes resetColour)
             in do
                    hWrite stdout text
        }

honeycomb :: Rope -> Exporter
honeycomb _ = emptyExporter
