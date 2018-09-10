{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

--import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as S
import qualified Data.HashMap.Strict as HashMap
import Data.String.Here
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Core.Text
import Core.Json
import Core.Program
import Core.System
import Core.Render

k = JsonKey "intro"
v = JsonString "Hello"

j = JsonObject (HashMap.fromList
        [ (k, v)
        , (JsonKey "song", JsonString "Thriller")
        , (JsonKey "other", JsonString "A very long name for the \"shadow of the moon\".")
        , (JsonKey "four", JsonObject (HashMap.fromList
                [ (JsonKey "n1", r)
                ]))
        ])

b = StrictBytes (S.pack "{\"cost\": 4500}")

r = JsonArray [JsonBool False, JsonNull, JsonNumber 42]

data Boom = Boom
    deriving Show

instance Exception Boom

program :: Program ()
program = do
    event "Starting..."

    params <- getCommandLine
    debugS "params" params

    name <- getProgramName
    debug "programName" name

    setProgramName "hello"

    name <- getProgramName
    debug "programName" name

    debug "key" (render k)
    event "Verify internal values"

    let x = encodeToUTF8 j
    writeS x

    let (Just y) = decodeFromUTF8 b
    writeS y
    writeS (encodeToUTF8 y)
    writeS (encodeToUTF8 r)

    debug "packet" (render j)

    event "Clock..."

    fork $ do
        sleep 1.5
        event "Wakey wakey"
        throw Boom

    replicateM_ 5 $ do
        sleep 0.5
        event "tick"


    event "Brr! It's cold"
    terminate 0


main :: IO ()
main = do
    context <- configure (simple
        [ Option "verbose" (Just 'v') [here|
            Turn on event level logging to console.
            Valid values are "event", "debug", and "none" (the default
            if you don't specify the verbose option).
          |]
        , Option "logging" Nothing [here|
            Valid values are "console", "file:/path/to/file.log", and "syslog"
          |]
        , Option "quiet" (Just 'q') [here|
            Supress normal output.
          |]
        , Argument "filename" [here|
            The file you want to frobnicate.
          |]
        ])

{-
    context1 <- configure (complex
        [ Global
            [ Option "verbose" (Just 'v') [here|
                  Turn on event level logging to console. 
                  Valid values are "event", "debug", and "none" (the default
                  if you don't specify the verbose option).
              |]
            , Option "logging" Nothing [here|
                  Valid values are "normal", "structured", and ...
              |]
            , Option "logging" Nothing [here|
                  Valid values are "console", "file:/path/to/file.log", and "syslog"
              |]
            , Option "quiet" (Just 'q') "Supress normal output."
            ]

        , Command "add" "Add a file" 
            [ Argument "filename" "File to add"
            ]

        , Command "commit" "Commit your changes to the repository" 
            [ Option "message" (Just 'm') "Specify commit message (instead of using editor)"
            ]
  
        , Environment
            [ Variable "CRAZY_MODE" "Specify how many crazies to activate"
            , Variable "GITHUB_TOKEN" "OAuth token to access GitHub"
            ]
        ])
-}  
    executeWith context program

{-
main3 :: IO ()
main3 = do
    context <- configure $ do
        options config
        logging Structured
-}


{-
main :: IO ()
main = do
    context <- configure $ do
        initLoggingToStash
        clearScreenBeforeRunning

    execute' context program
-}

