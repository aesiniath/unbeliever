{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckWebserverIntegration where

import Core.Program
import Network.Wai
import Core.Webserver.Warp

import Test.Hspec hiding (context)

{-
type Application =
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
-}

dummyApplication :: Application
dummyApplication = undefined

checkWebserverIntegration :: Spec
checkWebserverIntegration = do
    describe "Warp/WAI integration" $ do
        it "instantiates a webserver on a known port" $ do
            context <- configure "1" None blankConfig
            executeWith context $ do
                launchWebserver 54321 dummyApplication
                pure ()
