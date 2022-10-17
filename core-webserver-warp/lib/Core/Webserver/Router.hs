{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Machinery for routing HTTP requests to appropriate handler functions.

Most of the time when writing a webserver or backend service for an
application we need to handle multiple different request paths. Even if the
process handles a only single primary endpoint there is often still a
requirement to respond to requests for status and to have simple health checks
that can be used to inform load balancers that the service is available in
addition to that primary endpoint. Sending different requests to different
functions is called /routing/.

= Usage

This module provides a simple mechanism for declaring routes and their
corresponding targets. You do this by creating a list of "handlers", one for
each context path prefix you want to route requests to, then using the
'prepareRoutes' function to compile this list into a WAI 'Application' that
can be passed to 'Core.Webserver.Warp.launchWebserver'.

@
    application <- 'prepareRoutes'
        [ \"api\"
            '</>' [ \"check\" `'handleRoute'` checkHandler
                , \"servicename\"
                    '</>' [ \"v3\"
                            '</>' [ \"status\" `'handleRoute'` statusHandler
                                , \"update\" `'captureRoute'` updateHandler
                                ]
                        ]
                ]
        ]

    'Core.Webserver.Warp.launchWebserver' 80 application
@

This results in an HTTP server responding to the following routes:

- <http:\/\/www.example.com\/api\/check>

- <http:\/\/www.example.com\/api\/servicename\/v3\/status>

- <http:\/\/www.example.com\/api\/servicename\/v3\/update>

- <http:\/\/www.example.com\/api\/servicename\/v3\/update\/12345678/field>

Requests to any other paths (for example @\/api@ and
@\/api\/servicename\/v3@) will result in a @404 Not Found@ response.
-}
module Core.Webserver.Router (
    -- * Setup
    Route,
    Prefix,
    Remainder,
    literalRoute,
    handleRoute,
    captureRoute,
    (</>),

    -- * Compile
    prepareRoutes,

    -- * Internal
    notFoundHandler,
) where

import Control.Exception.Safe qualified as Safe
import Core.Program.Context (Program)
import Core.Program.Logging
import Core.Program.Unlift (subProgram)
import Core.Telemetry.Observability (metric, setSpanName, telemetry)
import Core.Text.Rope
import Core.Webserver.Warp (ContextNotFoundInRequest (..), contextFromRequest)
import Data.ByteString.Builder qualified as Builder
import Data.List qualified as List (foldl')
import Data.String (IsString (fromString))
import Data.Trie qualified as Trie
import Network.HTTP.Types (status404)
import Network.Wai (Application, Request (rawPathInfo), Response, ResponseReceived, responseBuilder)
import Prelude hiding ((+), (/))

type Prefix = Rope

type Remainder = Rope

{- |
Component of a context path in a URL request that can be routed to a hander in
the 'Program' @τ@ monad. Routes can be nested under other routes, building up
the familiar tree structure commonly used by webservers.

@since 0.2.0
-}
data Route τ = Route
    { routePrefix :: Prefix
    , routeHandler :: Prefix -> Remainder -> Request -> Program τ Response
    , routeChildren :: [Route τ]
    }

{- |
A segment of a route that is to be matched exactly. For example,

@
    'literalRoute' \"api\"
@

will match the context path @\/api@.

This is the used for the definition of the 'IsString' instance enabling use of
@OverloadedStrings@, so

@
    \"api\"
@

will /also/ match the context path @\/api@ and makes for cleaner routing
specifications when building up nested paths with '(</>)'.

@since 0.2.0
-}
literalRoute :: Prefix -> Route τ
literalRoute prefix =
    -- if this is the node that gets served, then it's 404 Not Found because, by definition, there wasn't an actual
    -- handler defined by the user!
    Route
        { routePrefix = prefix
        , routeHandler = (\_ _ request -> notFoundHandler request)
        , routeChildren = []
        }

{- |
Route a given prefix to the supplied handler. You specify the prefix that you
want covered, and if the request path a matches the handler function will be
invoked.

@
    'handleRoute' \"status\" statusHandler
@

will match the context path @/status@ and invoke your function called
@statusHandler@ when requests for this path come in.

@since 0.2.0
-}
handleRoute :: Prefix -> (Request -> Program τ Response) -> Route τ
handleRoute prefix handler =
    Route
        { routePrefix = prefix
        , routeHandler = (\_ _ request -> handler request)
        , routeChildren = []
        }

{- |
Route a given prefix to the supplied handler, passing any following components
of the path to that handler.

This is a more general variation of 'handleRoute' which allows you to
\"capture\" the part of the context path that came /after/ the route prefix,
if there is one (and an empty string otherwise).

For example,

@
    'captureRoute' \"person\"
@

will match the context paths in the URLs like these:

- <http:\/\/www.example.com\/person>

- <http:\/\/www.example.com\/person\/U37gcRTh>

- <http:\/\/www.example.com\/person\/U37gcRTh\/name>

In the case of the third example the result of matching on this 'Route' would
have a prefix of @\/person@ and a remainder of @\/U37gcRTh\/name@.

@since 0.2.0
-}
captureRoute :: Prefix -> (Prefix -> Remainder -> Request -> Program τ Response) -> Route τ
captureRoute prefix0 handler =
    Route
        { routePrefix = prefix0
        , routeHandler =
            ( \prefix remainder request -> do
                    setSpanName prefix
                    handler prefix remainder request
            )
        , routeChildren = []
        }

{- |
A default handler for routes that are encountered that don't have actual
handlers defined. This is what is served if the user requests an endpoint that
is defined by a 'literalRoute' or if the user requests a path that does not
route successfully..

@since 0.2.0
-}
notFoundHandler :: Request -> Program τ Response
notFoundHandler _ = do
    pure (responseBuilder status404 [] (Builder.stringUtf8 "Not Found"))

instance IsString (Route τ) where
    fromString :: String -> Route τ
    fromString = literalRoute . packRope

{- |
Nest a set of routes below a parent. This will take the prefix inherited to
this point and insert it in front of the prefixes of each of the `Route`s
listed as children.

@since 0.2.0
-}
(</>) :: Route τ -> [Route τ] -> Route τ
(</>) parent children =
    parent
        { routeChildren = children
        }

{- |
Compile a list of route handlers into a WAI 'Application' suitable to be
passed to 'Core.Webserver.Warp.launchWebserver'.

Internally this builds up a patricia tree of the different route prefixes.
Incoming requests are matched against these possibilities, and either the
corresponding handler is invoked or @404 Not Found@ is returned.

@since 0.2.0
-}
prepareRoutes :: [Route τ] -> Program τ Application
prepareRoutes routes = do
    let trie = buildTrie emptyRope routes
    pure (makeApplication trie)

buildTrie :: Prefix -> [Route τ] -> Trie.Trie (Prefix -> Remainder -> Request -> Program τ Response)
buildTrie prefix0 routes =
    List.foldl' f Trie.empty routes
  where
    f ::
        Trie.Trie (Prefix -> Remainder -> Request -> Program τ Response) ->
        Route τ ->
        Trie.Trie (Prefix -> Remainder -> Request -> Program τ Response)
    f trie (Route prefix1 handler children) =
        let prefix' = prefix0 <> singletonRope '/' <> prefix1
            trie1 = Trie.insert (fromRope prefix') handler trie
         in case children of
                [] -> trie1
                _ -> Trie.unionL trie1 (buildTrie prefix' children)

-- We invoke makeApplication here partially applied in order to return an
-- Application,
--
--              :: Trie.Trie (Request -> Program τ Response) -> Application
--
-- but we expand out the signature in here in full in order to understand
-- where ther request object and response functions come from.
makeApplication :: Trie.Trie (Prefix -> Remainder -> Request -> Program τ Response) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
makeApplication trie request sendResponse = do
    let possibleContext = contextFromRequest request

    context <- case possibleContext of
        Nothing -> Safe.throw ContextNotFoundInRequest
        Just value -> pure value

    let path = rawPathInfo request

    --
    -- And now the magic at the heart of this module. Data.Trie's match function
    -- looks up the longest entry in the trie that matches the supplied path.
    --

    let possibleRoute = Trie.match trie path

    case possibleRoute of
        Nothing -> do
            response <- subProgram context $ do
                notFoundHandler request
            sendResponse response
        Just (prefix', handler, remainder') -> do
            response <- subProgram context $ do
                let prefix = intoRope prefix'
                let remainder = intoRope remainder'
                internal ("prefix = " <> prefix)
                internal ("remainder = " <> remainder)
                telemetry
                    [ metric "request.route" prefix
                    ]

                handler prefix remainder request

            sendResponse response
