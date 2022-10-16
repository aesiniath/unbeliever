{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Control.Exception (bracket_)
import Core.Program
import Core.System
import Core.Telemetry
import Core.Text
import Core.Webserver.Router
import Core.Webserver.Warp
import Data.Trie qualified as Trie
import Network.HTTP.Types
import Network.Wai

-- type Application =
-- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

-- type Application =
--     Request ->
--     (Response -> IO ResponseReceived) ->
--     IO ResponseReceived

{-
demoApplication :: Application
demoApplication request handler = do
    rawPathInfo request

exampleApplication :: Application
exampleApplication req respond =
    bracket_
        (putStrLn "Allocating scarce resource")
        (putStrLn "Cleaning up")
        (respond (responseLBS status200 [] "Hello World"))
-}

exampleApplication :: Application
exampleApplication request sendResponse =
    let path = intoRope (rawPathInfo request)
        query = intoRope (rawQueryString request)
        path' = fromRope (path <> query)
     in do
            sendResponse (responseLBS status200 [] path')

port :: Port
port = 48080

main :: IO ()
main = do
    context <- configure "1" None (simpleConfig [])
    context' <- initializeTelemetry [consoleExporter, structuredExporter, honeycombExporter] context
    executeWith context' $ do
        info "Starting..."
        debugS "port" port
        launchWebserver port exampleApplication
