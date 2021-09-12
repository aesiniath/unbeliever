module Core.Program.Telemetry (
    eventT,
    debugT,
    Telemetry (..),
    withSpan,
) where

import Core.Program.Execute
import Core.Text.Bytes
import Core.Text.Rope

eventT :: Telemetry σ => σ -> Program τ ()
eventT = undefined

debugT :: Telemetry σ => Rope -> σ -> Program τ ()
debugT = undefined

class Telemetry σ where
    serialize :: σ -> Bytes

withSpan :: Rope -> Program τ α  -> Program τ α
withSpan = undefined
