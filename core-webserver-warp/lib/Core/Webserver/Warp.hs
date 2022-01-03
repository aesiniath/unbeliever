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

= Usage

First set up your program and initialize the telemetry subsystem.

@
import "Core.Program"
import "Core.Telemetry"
import "Core.Webservice.Warp"

main :: 'IO' ()
main = do
    context <- 'Core.Program.Execute.configure' \"1.0\" 'Core.Program.Execute.None' ('Core.Program.Arguments.simpleConfig' [])
    context' <- 'initializeTelemetry' ['Core.Telemetry.Console.consoleExporter', 'Core.Telemetry.Structured.structuredExporter', 'Core.Telemetry.Honeycomb.honeycombExporter'] context
    'Core.Program.Execute.executeWith' context' \$ do
        'Core.Program.Logging.info' \"Starting...\"
        'launchWebserver' 8080 application
@

You can then describe your webservice 'Application', for example

@
application :: 'Application'
application = request sendResponse =
    sendResponse ('Network.WAI.responseLBS' 'Network.HTTP.Types.status200' [] \"Hello World\")
@

performs the heroic duty of replying to you with the given string. In
practice, if you're using something like __servant__ to define the shape of
your webservice its 'Servant.serve' function will give you the 'Application'
you're trying to run.

Logging output is sent to the telemtry channel. If you run your program with
the console exporter, and hit something like
<http://localhost:8080/hello?question=answer> will see something like this:

@
\$ __hello-service --telemetry=console__
03:16:01Z (00.002) Starting...
03:16:04Z (00.259)                                             <-- this is the request duration, 259 ms
/hello:                                                        <-- the base of the context path aka \"endpoint\"
  request.method = \"GET\"
  request.path = "/hello?question=answer"                      <-- the full context path with query string
  response.status_code = "200"
@

This is useful for debugging during development but for production you are
recommended to use the structured logging output or to send the traces to an
observability service; this will be the root span of a trace.
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
import Core.Program.Logging
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
launchWebserver port application = do
    context <- getContext
    let settings =
            Warp.setOnException
                (onExceptionHandler context)
                . Warp.setPort port
                $ Warp.defaultSettings
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

onExceptionHandler :: Context τ -> Maybe Request -> SomeException -> IO ()
onExceptionHandler context possibleRequest e = do
    subProgram context $ do
        critical "Exception escaped webserver"
        debugS "e" e
        case possibleRequest of
            Nothing -> pure ()
            Just request ->
                let line = "\"" <> intoRope (requestMethod request) <> " " <> intoRope (rawPathInfo request) <> intoRope (rawQueryString request) <>  "\""
                 in debug "request" line
