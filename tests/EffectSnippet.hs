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
main = execute $ do
    context <- getContext
    liftIO $ do
        Effect.runEff $ do
            --         Effect.runEnvironment $ do
            --             Effect.getProgName
            -- debugS "name" name
            runProgramE context $ do
                thing

    write "Done"

thing :: (Effect.IOE :> es, ProgramE τ :> es) => Effect.Eff es ()
thing = do
    runProgram' $ \runEffect -> do
        info "Running in Program"

        name <- runEffect $ do
            -- an example of something that needs IOE
            Effect.runEnvironment $ do
                Effect.getProgName

        debugS "name" (name :: String)

        info "Done running effects"

        pure ()
