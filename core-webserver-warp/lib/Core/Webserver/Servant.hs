{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Core.Webserver.Servant (
    launchServantRoutes,
) where

import Control.Exception.Safe (try)
import Control.Monad.Except (ExceptT (..))
import Core.Program
import Core.System (Exception (..), throw)
import Core.Webserver.Warp
import Data.Proxy (Proxy)
import GHC.Base (Type)
import Network.Wai (Application)
import Servant (Handler (..), ServerT)
import Servant.Server (HasServer, hoistServer, serve)

data ContextNotFoundInRequest = ContextNotFoundInRequest deriving (Show)

instance Exception ContextNotFoundInRequest where
    displayException _ = "Context was not found in request. This is a serious error."

natTransform :: Context τ -> Program τ α -> Handler α
natTransform context program =
    let output =
            try $
                subProgram context program
     in Handler (ExceptT output)

launchServantRoutes ::
    forall τ (api :: Type).
    (HasServer api '[]) =>
    Port ->
    Proxy api ->
    ServerT api (Program τ) ->
    Program τ ()
launchServantRoutes port proxy (routes :: ServerT api (Program τ)) =
    let application :: Application
        application = \request sendResponse -> do
            -- The type application in `contextFromRequest` is important, as otherwise
            -- the compiler cannot infer that the type of `natTransform` is of the same
            -- `τ` as the one in `Program τ`
            context <- case contextFromRequest @τ request of
                Just ctx -> pure ctx
                -- This will happen in the case where the wiring has not been done
                -- appropriately. This code depends on what `launchWebserver` does,
                -- if it were to stop introducing the context in the `request`
                -- appropriately, then we'll get this error.
                Nothing -> throw ContextNotFoundInRequest
            serve
                proxy
                (hoistServer proxy (natTransform context) routes)
                request
                sendResponse
     in launchWebserver port application
