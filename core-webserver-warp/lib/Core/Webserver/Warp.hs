{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune #-}

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
    requestContextKey,
    contextFromRequest,
) where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import qualified Control.Exception.Safe as Safe (catch)
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
import qualified Data.ByteString.Lazy as L
import qualified Data.Vault.Lazy as Vault
import Network.HTTP.Types (
    hContentType,
    status400,
    status413,
    status431,
    status500,
    statusCode,
 )
import Network.HTTP2.Frame (
    ErrorCodeId (UnknownErrorCode),
    HTTP2Error (ConnectionError),
 )
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest, Port)
import qualified Network.Wai.Handler.Warp as Warp

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
        Warp.runSettings
            settings
            ( loggingMiddleware
                context
                application
            )

requestContextKey :: forall t. Vault.Key (Context t)
requestContextKey = unsafePerformIO Vault.newKey
{-# NOINLINE requestContextKey #-}

contextFromRequest :: forall t. Request -> Maybe (Context t)
contextFromRequest request = Vault.lookup requestContextKey (vault request)

-- which is IO
loggingMiddleware :: Context τ -> Application -> Application
loggingMiddleware (context0 :: Context τ) application request sendResponse = do
    let path = intoRope (rawPathInfo request)

    subProgram context0 $ do
        beginTrace $ do
            encloseSpan path $ do
                context1 <- getContext

                -- we could call `telemetry` here with these values, but since
                -- we call into nested actions which could clear the state
                -- without starting a new span, we duplicate adding them below
                -- to ensure they get passed through.

                let query = intoRope (rawQueryString request)
                    path' = path <> query
                    method = intoRope (requestMethod request)

                liftIO $ do
                    -- The below wires the context in the request's `vault`. As the type of
                    -- `Context` is polymorphic to support user data, we have to use a type
                    -- application to make sure that consumers can later fetch the appropriate
                    -- `Context t`.
                    let vault' = Vault.insert (requestContextKey @τ) context1 (vault request)
                        request' = request{vault = vault'}
                    Safe.catch
                        ( application request' $ \response -> do
                            -- accumulate the details for logging
                            let status = intoRope (show (statusCode (responseStatus response)))

                            subProgram context1 $ do
                                telemetry
                                    [ metric "request.method" method
                                    , metric "request.path" path'
                                    , metric "response.status_code" status
                                    ]

                            -- actually handle the request
                            sendResponse response
                        )
                        ( \(e :: SomeException) -> do
                            -- set the magic `error` field with the exception text.
                            let text = intoRope (displayException e)
                            subProgram context1 $ do
                                warn "Trapped internal exception"
                                debug "e" text
                                telemetry
                                    [ metric "request.method" method
                                    , metric "request.path" path'
                                    , metric "error" text
                                    ]

                            sendResponse (onExceptionResponse e)
                        )

onExceptionResponse :: SomeException -> Response
onExceptionResponse e
    | Just (_ :: InvalidRequest) <-
        fromException e =
        responseLBS
            status400
            [(hContentType, "text/plain; charset=utf-8")]
            (fromRope ("Bad Request\n" <> intoRope (displayException e)))
    | Just (ConnectionError (UnknownErrorCode 413) t) <-
        fromException e =
        responseLBS
            status413
            [(hContentType, "text/plain; charset=utf-8")]
            (L.fromStrict t)
    | Just (ConnectionError (UnknownErrorCode 431) t) <-
        fromException e =
        responseLBS
            status431
            [(hContentType, "text/plain; charset=utf-8")]
            (L.fromStrict t)
    | otherwise =
        responseLBS
            status500
            [(hContentType, "text/plain; charset=utf-8")]
            "Internal Server Error"

--
-- Ideally this would be a catch-all and not be hit; our application wrapper
-- should have caught this beforehand. However, it turns out "Bad Request"
-- type protocol problems that don't even result in a coherent request -
-- which, sadly, are somewhat to be expected on the wild and wooly internet.
-- So we note them briefly and move on.
--
-- Much more interesting are exceptions which occur within the request path,
-- which means that we can annotate the current span with an `error` field and
-- send it down the telemetry channel.
--
onExceptionHandler :: Context τ -> Maybe Request -> SomeException -> IO ()
onExceptionHandler context possibleRequest e = do
    subProgram context $ do
        critical "Exception escaped webserver"
        debugS "e" e
        case possibleRequest of
            Nothing -> pure ()
            Just request ->
                let line = intoRope (requestMethod request) <> " " <> intoRope (rawPathInfo request) <> intoRope (rawQueryString request)
                 in debug "request" line
