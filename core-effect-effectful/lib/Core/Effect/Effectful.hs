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

= Usage


The function 'runProgram' exported here is a the same name as that recommended
in "Core.Program.Unlift" for the inner function when calling 'withContext'.
There won't be a name collision but be aware of shadowing if using this
function.
-}
module Core.Effect.Effectful
    ( ProgramE
    , runProgram
    , runProgramE
    , runProgram'
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
    , withEffToIO
    )

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

runProgram'
    :: (Effect.IOE :> es, ProgramE τ :> es)
    => ((forall β. Effect.Eff es β -> Program τ β) -> Program τ α)
    -> Effect.Eff es α
runProgram' action = do
    -- extract Context τ
    ProgramE context <- Effect.getStaticRep

    -- lift to IO, using the provided specialized unlifting function
    Effect.withEffToIO $ \runInIO ->
        -- now in IO. Lift to Program τ
        subProgram context $ do
            -- now in Program τ. We form the function that will run an effect
            -- in Program, and pass it to the supplied action.
            action $ \inner -> do
                liftIO $ do
                    runInIO $ do
                        inner
