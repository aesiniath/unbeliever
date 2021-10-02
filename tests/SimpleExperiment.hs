{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

--import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Core.Data
import Core.Encoding
import Core.Program
import Core.System
import Core.Text
import qualified Data.ByteString.Char8 as S
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

k = JsonKey "intro"

v = JsonString "Hello"

j =
    JsonObject
        [ (k, v)
        , (JsonKey "song", JsonString "Thriller")
        , ("other", "A very long name for the \"shadow of the moon\".")
        ,
            ( JsonKey "four"
            , JsonObject
                [ (JsonKey "n1", r)
                ]
            )
        ]

b = intoBytes (S.pack "{\"cost\": 4500}")

r = JsonArray [JsonBool False, JsonNull, 42]

data Boom = Boom
    deriving (Show)

instance Exception Boom

program :: Program None ()
program = do
    info "Starting..."

    params <- getCommandLine
    debugS "params" params

    level <- getVerbosityLevel
    debugS "level" level

    name <- getProgramName
    debug "programName" name

    setProgramName "hello"

    name <- getProgramName
    debug "programName" name

    debugR "key" k
    info "Verify internal values"

    state <- getApplicationState
    debugS "state" state

    let x = encodeToUTF8 j
    writeS x

    let (Just y) = decodeFromUTF8 b
    writeS y
    writeS (encodeToUTF8 y)
    writeR (encodeToUTF8 y)
    writeS (encodeToUTF8 r)

    debugR "packet" j

    info "Clock..."

    forkThread $ do
        sleepThread 1.5
        warn "Wakey wakey"
        throw Boom

    replicateM_ 5 $ do
        sleepThread 0.5
        info "tick"

    info "Brr! It's cold"
    terminate 0

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <-
        configure
            version
            None
            ( simpleConfig
                [ Option
                    "quiet"
                    (Just 'q')
                    Empty
                    [quote|
            Supress normal output.
          |]
                , Argument
                    "filename"
                    [quote|
            The file you want to frobnicate.
          |]
                , Variable "HOME" "Home directory"
                ]
            )

    executeWith context program
