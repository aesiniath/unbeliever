{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

You use this by creating a list of "handlers", one for each context path
prefix you want to route requests to, then using the 'prepareRoutes' function
to compile this list into a WAI 'Application' that can be passed to
'Core.Webserver.Warp.launchWebserver'.

@
    application <- 'prepareRoutes'
        [ ("/update", updateHandler)
        , ("/status", statusHandler)
        ]
    'Core.Webserver.Warp.launchWebserver' 80 application
@
-}
module Core.Webserver.Router (
    Route,
    literalRoute,
    handleRoute,
    captureRoute,
    prepareRoutes,
    (</>),
    (<~>),
    (<:>),
) where

import Control.Exception.Safe qualified as Safe
import Core.Program.Context (Program)
import Core.Program.Logging
import Core.Program.Unlift (subProgram)
import Core.Text.Rope
import Core.Webserver.Warp (ContextNotFoundInRequest (..), contextFromRequest)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.List qualified as List (foldl')
import Data.String (IsString (fromString))
import Data.Trie qualified as Trie
import Network.HTTP.Types (status404)
import Network.Wai (Application, Request (rawPathInfo), Response, ResponseReceived, responseBuilder)
import Prelude hiding ((+), (/))

data Route τ = Route ByteString (ByteString -> Request -> Program τ Response)

{- |
A segment of a route that is to be matched exactly. For example,

@
    'literalRoute' \"api\"
@

will match the context path @/url@.

@since 0.1.2
-}
literalRoute :: Rope -> Route τ
literalRoute prefix =
    Route
        (fromRope prefix)
        -- if this is the node that gets served, then it's 404 Not Found because, by definition, there wasn't an actual
        -- handler defined by the user!
        (\_ request -> notFoundHandler request)

handleRoute :: Rope -> (Request -> Program τ Response) -> Route τ
handleRoute prefix handler =
    Route
        (fromRope prefix)
        (\_ request -> handler request)

captureRoute :: Rope -> (Rope -> Request -> Program τ Response) -> Route τ
captureRoute prefix handler =
    Route
        (fromRope prefix)
        ( \remainder request ->
            handler (intoRope remainder) request
        )

{- |
A default handler for routes that are encountered that don't have actual
handlers defined.
-}
notFoundHandler :: Request -> Program τ Response
notFoundHandler = undefined

instance IsString (Route τ) where
    fromString :: String -> Route τ
    fromString = literalRoute . packRope

(</>) :: Route τ -> [Route τ] -> Route τ
(</>) parent children = undefined

(<~>) :: Rope -> (Request -> Program τ Response) -> Route τ
(<~>) = handleRoute

(<:>) :: Rope -> (Rope -> Request -> Program τ Response) -> Route τ
(<:>) = captureRoute

{-
data Route =
 = Route Route
 | Branch [Route]
 | Handler (Request -> Program τ Response)
 | Capture (Request -> ByteString -> Program τ Response)

prepareRoutes0 :: [(Rope,  Request -> Program τ Response)] -> Program τ Application
prepareRoutes0 routes = do
    let routes' = fmap (\(route, handler) -> (fromRope route, handler)) routes
    let tree = Trie.fromList routes'

    pure (makeApplication tree)
-}

{- |
Compile a list of route handlers into a WAI 'Application'.

@since 0.1.2
-}
prepareRoutes :: [Route τ] -> Program τ Application
prepareRoutes routes = do
    -- let routes' = fmap (\(route, handler) -> (fromRope route, handler)) routes
    -- let tree = Trie.fromList routes'
    let trie = buildTrie routes

    pure (makeApplication trie)

buildTrie :: [Route τ] -> Trie.Trie (ByteString -> Request -> Program τ Response)
buildTrie routes = List.foldl' f Trie.empty routes
  where
    f ::
        Trie.Trie (ByteString -> Request -> Program τ Response) ->
        Route τ ->
        Trie.Trie (ByteString -> Request -> Program τ Response)
    f trie (Route prefix handler) = Trie.insert prefix handler trie

-- We invoke makeApplication here partially applied in order to return an
-- Application,
--
--              :: Trie.Trie (Request -> Program τ Response) -> Application
--
-- but we expand out the signature in here in full in order to understand
-- where ther request object and response functions come from.
makeApplication :: Trie.Trie (ByteString -> Request -> Program τ Response) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
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
            sendResponse (responseBuilder status404 [] (Builder.byteString path))
        Just (route, handler, remainder) -> do
            response <- subProgram context $ do
                internal ("matched = " <> intoRope route)
                internal ("remainder = " <> intoRope remainder)
                handler remainder request
            sendResponse response
