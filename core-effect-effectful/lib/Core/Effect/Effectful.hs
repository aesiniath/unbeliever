{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Effect systems are being actively explored as ways to structure Haskell
programs. This package provides experimental support for the __effectful__
effect system.

This module introcuces a new effect, 'ProgramE', which plumbs the current
program context into the effect system. By calling 'runProgramE' to add the
'ProgramE' effect to the current list of in-scope effects you can then use
'withProgram'' to run a 'Program' action. The more general 'withProgram' gives
you an unlifting function to return back to the effect system so you can
continue processing within your effects stack.

= Usage

As an example, here's an effect which lifts to 'Program' @τ@, does some log
output, then unlifts back to 'Effectful.Internal.Monad.Eff' @es@ to then query
something from the 'Effectful.Environment.Environment' effect (which requires
the 'Effectful.Internal.Monad.IOE' effect to run):

@
retrieveProgramName
    :: forall τ es
     . ('Effectful.Internal.Monad.IOE' 'Effectful.Internal.Effect.:>' es, 'ProgramE' τ 'Effectful.Internal.Effect.:>' es)
    => 'Effectful.Internal.Monad.Eff' es ()
retrieveProgramName = do
    -- we're in (IOE :> es, ProgramE :> es) => Eff es, right?

    'withProgram' @τ $ \\runEffect -> do
        -- now we're in Program τ

        'Core.Program.Logging.info' \"Running in Program\"

        path <- runEffect $ do
            -- now back in (IOE :> es, ProgramE τ :> es) => Eff es, and can call
            -- something that requires the IOE effect be present.

            'Effectful.Environment.runEnvironment' $ do
                -- now in (Environment :> es) => Eff es
                'Effectful.Environment.getExecutablePath'

        'Core.Program.Logging.info' \"Done running effects\"
        'Core.Program.Logging.debugS' "path" path
@

The @@τ@ type application shown here is vital; without it the compiler will
not be able to resolve all the ambiguous types when attempting to determine
which effect to run. It doesn't have to be polymorphic; if you know the actual
top-level application state type you can do @@Settings@ or whatever.

This all assumes you are running with the 'ProgramE' @τ@ effect in-scope. You
can achieve that as follows:

@
main :: 'IO' ()
main = 'Core.Program.Execute.execute' program

program :: 'Program' 'None' ()
program = do
    -- in Program τ, where τ is None here
    context <- 'Core.Program.Execute.getContext'
    'Control.Monad.IO.Class.liftIO' $ do
        -- in IO
        'Effectful.Internal.Monad.runEff' $ do
            -- in (IOE :> es) => Eff es
            'runProgramE' context $ do
                -- in (IOE :> es, ProgramE τ :> es) => Eff es
                ...
@
-}
module Core.Effect.Effectful
    ( -- * Effect
      ProgramE
    , runProgramE

      -- * Lifting and unlifting
    , withProgram
    , withProgram'
    )
where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Core.Program.Context
import Core.System.Base
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
    , putStaticRep
    , withEffToIO
    )

-------------------------------------------------------------------------------
-- Effect

{- |
An effect giving you access to return to the 'Program' @τ@ monad.
-}
data ProgramE (τ :: Type) :: Effect.Effect

type instance Effect.DispatchOf (ProgramE τ) = 'Effect.Static 'Effect.WithSideEffects
newtype instance Effect.StaticRep (ProgramE τ) = ProgramE (Context τ)

-------------------------------------------------------------------------------
-- Interpretation

{- |
Given you are in the 'Effectful.Internal.Monad.IOE' effect, raise the
currently in-scope effects to include the 'ProgramE' effect. This will
presumably be invoked fairly soon after entering the effect system, and it
needs to have been done inside a program that was started with
'Core.Program.Execute.execute' or 'Core.Program.Execute.executeWith'. Assuming
that to be the case, get the 'Context' @τ@ object from the outside edge of
your program using 'getContext' and then provide it to this function at the
earliest opportunity.
-}
runProgramE
    :: forall τ es α
     . Effect.IOE :> es
    => Context τ
    -> Effect.Eff (ProgramE τ : es) α
    -> Effect.Eff es α
runProgramE context = Effect.evalStaticRep (ProgramE context)

--------------------------------------------------------------------------------
-- Wrappers

{- |
Simple variant of 'withProgram' which allows you to run a 'Program' @τ@ monad
action from within the effect system, provided that the 'ProgramE' @τ@ effect
is in scope.
-}
withProgram'
    :: forall τ es α
     . (Effect.IOE :> es, ProgramE τ :> es)
    => Program τ α
    -> Effect.Eff es α
withProgram' action = do
    ProgramE context1 <- Effect.getStaticRep

    -- lift to IO
    liftIO $ do
        -- now in IO. Lift to Program τ
        subProgram context1 $ do
            action

{- |
Run a 'Program' @τ@ monad action within the 'ProgramE' @τ@ effect.

This allows you the ability to lift to the 'Program' @τ@ monad, giving you the
ability to run actions that do logging, telemetry, input/output, and exception
handling, and then unlift back to the 'Eff' @es@ effect to continue work in
the effects system.

The order of the existential types in the @forall@ turned out to matter; it
allows you to use the @TypeApplications@ language extention to resolve the
ambiguous types when invoking this function.

See also "Core.Program.Unlift" for a general discussion of the unlifting
problem and in particular the 'Core.Program.Unlift.withContext' for a function
with a comparable type signature.
-}
withProgram
    :: forall τ es α
     . (Effect.IOE :> es, ProgramE τ :> es)
    => ((forall β. Effect.Eff es β -> Program τ β) -> Program τ α)
    -> Effect.Eff es α
withProgram action = do
    -- extract Context τ
    ProgramE context1 <- Effect.getStaticRep

    -- lift to IO, using the provided specialized function which gives an unlift
    -- function for later use.
    Effect.withEffToIO $ \runInIO ->
        -- now in IO. Lift to Program τ
        subProgram context1 $ do
            -- now in Program τ. We form the function that will run an effect
            -- in Program τ, and pass it to the supplied action.
            action $ \inner -> do
                context2 <- getContext
                liftIO $ do
                    -- now in IO
                    runInIO $ do
                        -- now in (IOE :> es, ProgramE :> es) => Eff es, but
                        -- we need to update the Context τ in the ProgramE τ
                        -- effect with the one that was present just before we
                        -- ran the unlifting function.
                        Effect.putStaticRep (ProgramE context2)

                        -- now we can proceed with running the nested effects.
                        inner
