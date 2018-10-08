{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Core.Text
import Core.Encoding
import Core.Program
import Core.System

k = JsonKey "intro"
v = JsonString "Hello"

j = JsonObject
        [ (k, v)
        , (JsonKey "song", JsonString "Thriller")
        , ("other", "A very long name for the \"shadow of the moon\".")
        , (JsonKey "four", JsonObject (HashMap.fromList
                [ (JsonKey "n1", r)
                ]))
        ]

b = StrictBytes (S.pack "{\"cost\": 4500}")

r = JsonArray [JsonBool False, JsonNull, 42]

data Boom = Boom
    deriving Show

instance Exception Boom

program :: Program None ()
program = do
    event "Starting..."

    params <- getCommandLine
    debugS "params" params

    name <- getProgramName
    debug "programName" name

    setProgramName "hello"

    name <- getProgramName
    debug "programName" name

    debugR "key" k
    event "Verify internal values"

    state <- getApplicationState
    debugS "state" state

    let x = encodeToUTF8 j
    writeS x

    let (Just y) = decodeFromUTF8 b
    writeS y
    writeS (encodeToUTF8 y)
    writeS (encodeToUTF8 r)

    debugR "packet" j

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
    context <- configure None (simple
        [ Option "verbose" (Just 'v') [quote|
            Turn on event level logging to console.
            Valid values are "event", "debug", and "none" (the default
            if you don't specify the verbose option).
          |]
        , Option "logging" Nothing [quote|
            Valid values are "console", "file:/path/to/file.log", and "syslog".
          |]
        , Option "quiet" (Just 'q') [quote|
            Supress normal output.
          |]
        , Argument "filename" [quote|
            The file you want to frobnicate.
          |]
        , Variable "HOME" "Home directory"
        ])

    executeWith context program
