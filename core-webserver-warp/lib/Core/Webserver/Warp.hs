{-# LANGUAGE OverloadedStrings #-}

module Core.Webserver.Warp where

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
    subProgram context0 $ do
        beginTrace $ do
            encloseSpan "Handle request" $ do
                context1 <- getContext
                liftIO $ do
                    application request $ \response -> do
                        let path = intoRope (rawPathInfo request)
                            query = intoRope (rawQueryString request)
                            method = intoRope (requestMethod request)
                            status = intoRope (show (statusCode (responseStatus response)))

                        -- actually handle the request
                        result <- sendResponse response

                        -- accumulate the details for logging
                        subProgram context1 $ do
                            telemetry
                                [ metric "request.method" method
                                , metric "request.path" path
                                , metric "request.query" query
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
