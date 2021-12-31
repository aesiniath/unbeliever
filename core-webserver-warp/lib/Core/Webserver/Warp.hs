-- |
module Core.Webserver.Warp where

--
-- We follow the convention used elsewhere in this collection of libraries of
-- using a qualified name for the imports of significant libraries. It's a bit
-- cumbersome, but makes it easier to disambiguate what's going on when
-- comparing to almost identical code in sibling modules covering other
-- webserver frameworks.
--

import Core.Program.Execute (Program)
import Core.System.Base

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

type Port = Int

launchWebserver :: Port -> Application -> Program τ ()
launchWebserver port application =
    let settings =
            Warp.setOnException
                onExceptionHandler
                . Warp.setPort port
                $ Warp.defaultSettings
     in do
            liftIO $ do
                Warp.runSettings settings (loggingMiddleware application)
  where
    loggingMiddleware :: Application -> Application
    loggingMiddleware = logStdoutDev

onExceptionHandler :: Maybe Request -> SomeException -> IO ()
onExceptionHandler possibleRequest e = do
    Warp.defaultOnException possibleRequest e
