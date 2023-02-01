{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Core.Effect.Effectful
import Core.Program
import Core.System
import Core.Text
import Data.ByteString.Char8 qualified as C
import Effectful qualified as Effect
import Effectful.Environment qualified as Effect
import Effectful.Internal.Effect (type (:>))

main :: IO ()
main = execute program

program :: Program None ()
program = do
    -- in Program τ
    context <- getContext
    liftIO $ do
        -- in IO
        Effect.runEff $ do
            -- in (IOE :> es) => Eff es
            runProgramE context $ do
                retrieveProgramName @None

    write "Done"

--
-- In order to test the withProgram unlifting mechanism, we wanted an example
-- of something that needs the IOE effect. The Environment effect fits the
-- bill.
--

retrieveProgramName
    :: forall τ es
     . (Effect.IOE :> es, ProgramE τ :> es)
    => Effect.Eff es ()
retrieveProgramName = do
    -- we're in (IOE :> es, ProgramE :> es) => Eff es, right?

    withProgram @τ $ \runEffect -> do
        -- now we're in Program τ
        info "Running in Program"

        path <- runEffect $ do
            -- now back in (IOE :> es, ProgramE :> es) => Eff es, and can call
            -- something that requires the IOE effect be present.

            Effect.runEnvironment $ do
                -- now in (Environment :> es) => Eff es
                Effect.getExecutablePath

        info "Done running effects"
        debugS "path" path

