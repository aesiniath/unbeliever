{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune #-}

{- |

= Usage

-}
module Core.Effect.Effectful
    ( ProgramE
    , runProgram
    )
where


import Control.Monad.IO.Unlift qualified as UnliftIO
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Telemetry.Identifiers
import Core.Telemetry.Observability
import Core.Text.Rope
import Effectful
    ( Dispatch (Static)
    , Effect
    )
import Effectful.Internal.Effect (type (:>>))
import Effectful.Internal.Env
    ( DispatchOf
    , SideEffects (WithSideEffects)
    )
import Effectful.Internal.Monad
    ( Eff
    , IOE
    , StaticRep
    , withEffToIO
    )

-------------------------------------------------------------------------------
-- Effect

data ProgramE (τ :: *) :: Effect

type instance DispatchOf (ProgramE τ) = Static WithSideEffects
newtype instance StaticRep (ProgramE τ) = ProgramE (Context τ)

-------------------------------------------------------------------------------
-- Interpretation

runProgramE
    :: '[IOE] :> es
    => Context τ
    -> Eff (ProgramE τ : es) α
    -> Eff es α
runProgramE context = Effect.evalStaticRep (ProgramE context)

--------------------------------------------------------------------------------
-- Wrappers

runProgram
    :: '[IOE, ProgramE τ] :>> es
    => Program τ a
    -> Eff es a
runProgram action = do
    ProgramE context <- Effect.getStaticRep
    liftIO $ subProgram context action

-- This function may be tweaked if for some reason we want a precise unlifting
-- strategy (seems related to threads and concurrency) by changing withEffToIO
-- to either withSeqEffToIO or withConcEffToIO.
runProgramEndo
    :: forall τ es a
     . (HasCallStack, '[IOE, ProgramE τ] :>> es)
    => (Program τ a -> Program τ a)
    -> Eff es a
    -> Eff es a
runProgramEndo endo eff = do
    ProgramE context <- Effect.getStaticRep
    withEffToIO @es $ \effToIO ->
        liftIO $
            subProgram context $
                UnliftIO.withRunInIO @(Program τ) $ \runInIO ->
                    runInIO $ endo $ UnliftIO.liftIO $ effToIO eff

encloseSpanEff
    :: forall τ es a
     . (HasCallStack, '[IOE, ProgramE τ] :>> es)
    => Label
    -> Eff es a
    -> Eff es a
encloseSpanEff label = runProgramEndo @τ (encloseSpan label)
