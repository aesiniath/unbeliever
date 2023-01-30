{-# LANGUAGE AllowAmbiguousTypes #-}
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
    , runProgramE
    )
where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Control.Monad.IO.Unlift qualified as UnliftIO
import Core.Program.Context
import Core.System.Base
import Core.Telemetry.Observability
import Data.Kind (Type)
import Effectful.Internal.Effect (type (:>))
import Effectful.Internal.Effect qualified as Effect
    ( Effect
    )
import Effectful.Internal.Env qualified as Effect
    ( Dispatch (Static)
    , DispatchOf
    , SideEffects (WithSideEffects)
    )
import Effectful.Internal.Monad qualified as Effect
    ( Eff
    , IOE
    , StaticRep
    , evalStaticRep
    , getStaticRep
    , withEffToIO
    )
import GHC.Stack (HasCallStack)

-------------------------------------------------------------------------------
-- Effect

data ProgramE (τ :: Type) :: Effect.Effect

type instance Effect.DispatchOf (ProgramE τ) = 'Effect.Static 'Effect.WithSideEffects
newtype instance Effect.StaticRep (ProgramE τ) = ProgramE (Context τ)

-------------------------------------------------------------------------------
-- Interpretation

runProgramE
    :: Effect.IOE :> es
    => Context τ
    -> Effect.Eff (ProgramE τ : es) α
    -> Effect.Eff es α
runProgramE context = Effect.evalStaticRep (ProgramE context)

--------------------------------------------------------------------------------
-- Wrappers

runProgram
    :: (Effect.IOE :> es, ProgramE τ :> es)
    => Program τ a
    -> Effect.Eff es a
runProgram action = do
    ProgramE context <- Effect.getStaticRep
    liftIO $ subProgram context action

-- This function may be tweaked if for some reason we want a precise unlifting
-- strategy (seems related to threads and concurrency) by changing withEffToIO
-- to either withSeqEffToIO or withConcEffToIO.
runProgramEndo
    :: forall τ es a
     . (HasCallStack, Effect.IOE :> es, ProgramE τ :> es)
    => (Program τ a -> Program τ a)
    -> Effect.Eff es a
    -> Effect.Eff es a
runProgramEndo endo eff = do
    ProgramE context <- Effect.getStaticRep
    Effect.withEffToIO @es $ \effToIO ->
        liftIO $
            subProgram context $
                UnliftIO.withRunInIO @(Program τ) $ \runInIO ->
                    runInIO $ endo $ UnliftIO.liftIO $ effToIO eff

encloseSpanEff
    :: forall τ es a
     . (HasCallStack, Effect.IOE :> es, ProgramE τ :> es)
    => Label
    -> Effect.Eff es a
    -> Effect.Eff es a
encloseSpanEff label = runProgramEndo @τ (encloseSpan label)
