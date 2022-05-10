{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Support integrating web services created by __servant__ with handlers defined
in the 'Program' monad. This is a thin wrapper which creates an 'Application'
which can be used with 'Core.Webserver.Warp.launchWebserver'.

@
import "Core.Program"
import "Core.Webserver.Servant"
import "Core.Webserver.Warp"

import MyServer (api, routes)

main :: 'IO' ()
main = do
    'Core.Program.Execute.execute' $ do
        application <- 'prepareRoutes' api routes
        'launchWebserver' 8080 application
@
-}
module Core.Webserver.Servant (
    prepareRoutes,
    prepareRoutesWithContext,
) where

import Control.Monad.Except (ExceptT (..))
import Core.Program
import Core.System (Exception (..))
import Core.Telemetry.Observability (clearMetrics)
import Core.Webserver.Warp
import Data.Proxy (Proxy)
import GHC.Base (Type)
import Network.Wai (Application)
import qualified Servant as Servant (
    Handler (..),
    ServerT,
 )
import qualified Servant.Server as Servant (
    Context (..),
    HasServer,
    ServerContext,
    serveWithContextT,
 )

data ContextNotFoundInRequest = ContextNotFoundInRequest deriving (Show)

instance Exception ContextNotFoundInRequest where
    displayException _ = "Context was not found in request. This is a serious error."

{- |
Convert a __servant__ API and set of handlers into a __warp__ 'Application'.

This 'Application' must be used with 'Core.Webserver.Warp.launchWebserver' so
that the necessary internal connections are made.

Usage is straight forward:

@
        application <- 'prepareRoutes' api routes
        'launchWebserver' 8080 application
@

This code creates an Application which has sufficient information to unlift
back to the 'Program' monad so that your handlers can be take advantage of the
logging and telemetry facilities of __core-program__ and __core-telemetry__.
-}
prepareRoutes ::
    forall τ (api :: Type).
    Servant.HasServer api '[] =>
    Proxy api ->
    Servant.ServerT api (Program τ) ->
    Program τ Application
prepareRoutes proxy = prepareRoutesWithContext proxy Servant.EmptyContext

{- |
Prepare routes as with 'prepareRoutes' above, but providing a __servant__ 'Context'.

@since 0.1.1
-}
prepareRoutesWithContext ::
    forall τ (api :: Type) context.
    (Servant.HasServer api context, Servant.ServerContext context) =>
    Proxy api ->
    Servant.Context context ->
    Servant.ServerT api (Program τ) ->
    Program τ Application
prepareRoutesWithContext proxy sContext (routes :: Servant.ServerT api (Program τ)) =
    pure application
  where
    application :: Application
    application = \request sendResponse -> do
        -- The type application in `contextFromRequest` is important, as
        -- otherwise the compiler cannot infer that the type of
        -- `transformProgram` is of the same `τ` as the one in `Program τ`

        -- This exception will happen in the case where this is not being run
        -- by `launchWebserver`, since we need the Context to be stashed in
        -- the request by the Middleware there.

        context <- case contextFromRequest @τ request of
            Just context' -> pure context'
            Nothing -> throw ContextNotFoundInRequest
        Servant.serveWithContextT
            proxy
            sContext
            (transformProgram context)
            routes
            request
            sendResponse

    transformProgram :: Context τ -> Program τ α -> Servant.Handler α
    transformProgram context program =
        let output =
                try $
                    subProgram context $ do
                        clearMetrics
                        program
         in Servant.Handler (ExceptT output)
