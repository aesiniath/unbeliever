{-# LANGUAGE ImportQualifiedPost #-}
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
module Core.Webserver.Warp
    ( Port
    , launchWebserver
    , launchWebserverTLS
    , requestContextKey
    , contextFromRequest
    , ContextNotFoundInRequest (..)
    ) where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Control.Exception.Safe qualified as Safe (catch)
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Telemetry.Identifiers
import Core.Telemetry.Observability
import Core.Text.Rope
import Data.List qualified as List
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types
    ( Status
    , hContentType
    , status400
    , status413
    , status431
    , status500
    , statusCode
    )
import Network.HTTP2.Frame
    ( ErrorCodeId (UnknownErrorCode)
    , HTTP2Error (ConnectionError)
    )
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest, Port)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings)
import Network.Wai.Handler.WarpTLS qualified as Warp

{- |
Given a WAI 'Application', run a Warp webserver on the specified port from
within the 'Program' monad.

@
    'launchWebserver' 80 application
@

(this wraps the __warp__ package)
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

{- |
Given a WAI 'Application', run a Warp webserver on the specified port from
within the 'Program' monad. This variant of 'launchWebserver' runs the server
with a TLS connection.

For the common case of supplying a certificate and private key, you can do:

@
    let crypto = 'tlsSettings' \"\/path\/to\/certificate.crt\" \"\/path\/to\/private.key\"
    'launchWebserverTLS' crypto 443 application
@

(this wraps the __warp-tls__ package; for more complex certificate management
requirements see the documentation for the 'TLSSettings' type there)

@since 0.2.1
-}
launchWebserverTLS :: TLSSettings -> Port -> Application -> Program τ ()
launchWebserverTLS crypto port application = do
    context <- getContext
    let settings =
            Warp.setOnException
                (onExceptionHandler context)
                . Warp.setPort port
                $ Warp.defaultSettings
    liftIO $ do
        Warp.runTLS
            crypto
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

data ContextNotFoundInRequest = ContextNotFoundInRequest deriving (Show)

instance Exception ContextNotFoundInRequest where
    displayException _ = "Context was not found in request. This is a serious error."

-- which is IO
loggingMiddleware :: Context τ -> Application -> Application
loggingMiddleware (context0 :: Context τ) application request sendResponse = do
    let path = intoRope (rawPathInfo request)
        query = intoRope (rawQueryString request)
        method = intoRope (requestMethod request)

    subProgram context0 $ do
        resumeTraceIf request $ do
            encloseSpan path $ do
                context1 <- getContext

                -- we could call `telemetry` here with the request values, but
                -- since we call into nested actions which could clear the
                -- state without starting a new span, we duplicate adding them
                -- below to ensure they get passed through.

                liftIO $ do
                    -- The below wires the context in the request's `vault`. As the type of
                    -- `Context` is polymorphic to support user data, we have to use a type
                    -- application to make sure that consumers can later fetch the appropriate
                    -- `Context t`.
                    let vault' = Vault.insert (requestContextKey @τ) context1 (vault request)
                        request' = request {vault = vault'}
                    Safe.catch
                        ( application request' $ \response -> do
                            -- accumulate the details for logging
                            let code = statusCode (responseStatus response)

                            subProgram context1 $ do
                                telemetry
                                    [ metric "request.method" method
                                    , metric "request.path" path
                                    , if nullRope query then metric "request.query" () else metric "request.query" query
                                    , metric "response.status_code" code
                                    ]

                            -- actually handle the request
                            sendResponse response
                        )
                        ( \(e :: SomeException) -> do
                            -- set the magic `error` field with the exception text.
                            let text = intoRope (displayException e)
                                (status, detail) = assignException e
                                code = statusCode status

                            subProgram context1 $ do
                                warn "Trapped internal exception"
                                debug "e" text
                                telemetry
                                    [ metric "request.method" method
                                    , metric "request.path" path
                                    , if nullRope query then metric "request.query" () else metric "request.query" query
                                    , metric "response.status_code" code
                                    , metric "error" text
                                    ]

                            sendResponse
                                ( responseLBS
                                    status
                                    [(hContentType, "text/plain; charset=utf-8")]
                                    (fromRope detail)
                                )
                        )

assignException :: SomeException -> (Status, Rope)
assignException e
    | Just (_ :: InvalidRequest) <-
        fromException e =
        (status400, intoRope (displayException e))
    | Just (ConnectionError (UnknownErrorCode 413) t) <-
        fromException e =
        (status413, intoRope t)
    | Just (ConnectionError (UnknownErrorCode 431) t) <-
        fromException e =
        (status431, intoRope t)
    | otherwise =
        (status500, "Internal Server Error")

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
                in  debug "request" line

{- |
Pull the Trace identifier and parent Span identifier out of the request
headers, if present. Resume using those values, otherwise start a new trace.
-}
resumeTraceIf :: Request -> Program z a -> Program z a
resumeTraceIf request action =
    case extractTraceParent request of
        Nothing -> do
            beginTrace action
        Just (trace, parent) -> do
            usingTrace trace parent action

--
-- This is wildly inefficient. Surely warp must provide a better way to search
-- header values?!?
--
extractTraceParent :: Request -> Maybe (Trace, Span)
extractTraceParent request =
    let headers = requestHeaders request
        possibleValue = List.lookup "traceparent" headers
    in  case possibleValue of
            Nothing -> Nothing
            Just value -> parseTraceParentHeader (intoRope value)
