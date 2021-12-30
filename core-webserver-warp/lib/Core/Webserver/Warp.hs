module Core.Webserver.Warp where

import Core.Program.Execute (Program)
import Core.System.Base

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
        liftIO $ do
            Warp.runSettings settings application
        pure ()

onExceptionHandler :: Maybe Request -> SomeException -> IO ()
onExceptionHandler possibleRequest e = do
    Warp.defaultOnException possibleRequest e
