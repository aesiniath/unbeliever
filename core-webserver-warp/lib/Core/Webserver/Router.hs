{-# LANGUAGE ImportQualifiedPost #-}
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
    prepareRoutes,
) where

import Control.Exception.Safe qualified as Safe
import Core.Program.Context (Program)
import Core.Program.Unlift (subProgram)
import Core.Text.Rope
import Core.Webserver.Warp (ContextNotFoundInRequest (..), contextFromRequest)
import Data.ByteString.Builder qualified as Builder
import Data.Trie qualified as Trie
import Network.HTTP.Types (status201, status404)
import Network.Wai (Application, Request (rawPathInfo), Response, ResponseReceived, responseBuilder)
import Core.Program.Logging

{- |
Compile a list of route handlers into a WAI 'Application'.

@since 0.1.2
-}
prepareRoutes :: [(Rope, Request -> Program τ Response)] -> Program τ Application
prepareRoutes routes = do
    let routes' = fmap (\(route, handler) -> (fromRope route, handler)) routes
    let tree = Trie.fromList routes'

    pure (makeApplication tree)

-- We invoke makeApplication here partially applied in order to return an
-- Application,
--
--              :: Trie.Trie (Request -> Program τ Response) -> Application
--
-- but we expand out the signature in here in full in order to understand
-- where ther request object and response functions come from. 
makeApplication :: Trie.Trie (Request -> Program τ Response) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
makeApplication tree request sendResponse = do
    let possibleContext = contextFromRequest request

    context <- case possibleContext of
        Nothing -> Safe.throw ContextNotFoundInRequest
        Just value -> pure value

    let path = rawPathInfo request

    --
    -- And now the magic at the heart of this module. Data.Trie's match function
    -- looks up the longest entry in the trie that matches the supplied path.
    --

    let possibleRoute = Trie.match tree path

    case possibleRoute of
        Nothing -> do
            sendResponse (responseBuilder status404 [] (Builder.byteString path))
        Just (route, handler, remainder) -> do
            response <- subProgram context $ do 
                internal ("matched = " <> intoRope route)
                internal ("remainder = " <> intoRope remainder)
                handler request
            sendResponse response
