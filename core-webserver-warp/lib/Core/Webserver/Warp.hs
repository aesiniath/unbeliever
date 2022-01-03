{-# LANGUAGE OverloadedStrings #-}

{- |
Many programs present their interface in the form of a webservice, be it
internet-facing, for internal use, or even just as a machine-local daemon. The
Haskell language has numerous frameworks for building webservices and a number
of high-quality HTTP server implementations. This module provides support for
the Web Application Interface from the __wai__ package and the __warp__
webserver.

Given an 'Network.Wai.Application' type (the definition of your web service) and a
'Network.Wai.Middleware' (which is just @Application -> Application@), run a the
"Core.Program"'s 'Core.Program.Execute.Program' monad. Metrics values (aka web
server logs) will be sent as key/value pairs via "Core.Telemetry".
-}
module Core.Webserver.Warp (
    Port,
    launchWebserver,
) where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Core.Program.Context
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
import Network.HTTP.Types (statusCode)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

type Port = Int

{- |
Given a WAI 'Application', run a Warp webserver on the specified port from
within the 'Program' monad.
-}
launchWebserver :: Port -> Application -> Program τ ()
launchWebserver port application =
    let settings =
            Warp.setOnException
                onExceptionHandler
                . Warp.setPort port
                $ Warp.defaultSettings
     in do
            context <- getContext
            liftIO $ do
                Warp.runSettings settings (loggingMiddleware context application)

-- which is IO
loggingMiddleware :: Context τ -> Application -> Application
loggingMiddleware context0 application request sendResponse = do
    let path = intoRope (rawPathInfo request)

    subProgram context0 $ do
        beginTrace $ do
            encloseSpan path $ do
                context1 <- getContext
                liftIO $ do
                    application request $ \response -> do
                        let query = intoRope (rawQueryString request)
                            path' = path <> query
                            method = intoRope (requestMethod request)
                            status = intoRope (show (statusCode (responseStatus response)))

                        -- actually handle the request
                        result <- sendResponse response

                        -- accumulate the details for logging
                        subProgram context1 $ do
                            telemetry
                                [ metric "request.method" method
                                , metric "request.path" path'
                                , metric "response.status_code" status
                                ]

                        pure result

{-
innerMiddleware :: Context τ -> Application -> Application
innerMiddleware context application request sendResponse = do
    subProgram context $ do
        info "inner"
        liftIO $ do
            application request sendResponse
-}
onExceptionHandler :: Maybe Request -> SomeException -> IO ()
onExceptionHandler possibleRequest e = do
    Warp.defaultOnException possibleRequest e
