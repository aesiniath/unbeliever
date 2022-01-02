-- |
module Core.Webserver.Warp where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
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
loggingMiddleware context application request sendResponse = do
    subProgram context $ do
        beginTrace $ do
            encloseSpan "Handle request" $ do
                let path = intoRope (rawPathInfo request)
                    query = intoRope (rawQueryString request)
                    method = intoRope (requestMethod request)
                telemetry
                    [ metric "http_request_method" method
                    , metric "http_request_path" path
                    , metric "http_request_query" query
                    ]
                liftIO $ do
                    application request sendResponse

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
