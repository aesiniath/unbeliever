module Core.Program.Telemetry (
    eventT,
    debugT,
    Telemetry (..),
    withSpan,
) where

import Core.Program.Execute
import Core.Text.Bytes
import Core.Text.Rope

eventT :: Telemetry a => a -> Program z ()
eventT = undefined

debugT :: Telemetry a => Rope -> a -> Program z ()
debugT = undefined

class Telemetry a where
    serialize :: a -> Bytes

withSpan :: Rope -> Program t a -> Program t a
withSpan = undefined
