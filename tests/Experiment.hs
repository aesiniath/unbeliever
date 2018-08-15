{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as S
import qualified Data.HashMap.Strict as HashMap

import Core.Text
import Core.Json
import Core.Program
import Core.System
import Core.Render

k = JsonKey "intro"
v = JsonString "Hello"
j = JsonObject (HashMap.fromList [(k, v)])

b = StrictBytes (S.pack "{\"cost\": 4500}")

r = JsonArray [JsonBool False, JsonNull, JsonNumber 42]

data Boom = Boom
    deriving Show

instance Exception Boom

program :: Program ()
program = do
    debug "Starting..."
    name <- getProgramName
    debug name

    setProgramName "hello"

    name <- getProgramName
    debug name

    write (render k)

    let x = encodeToUTF8 j
    writeS x

    let (Just y) = decodeFromUTF8 b
    writeS y
    writeS (encodeToUTF8 y)
    writeS r
    writeS (encodeToUTF8 r)

    write (render j)
    write (render r)

    debug "Clock..."

    fork $ do
        sleep 1.5
        throw Boom

    replicateM_ 5 $ do
        sleep 0.5
        debug "tick"


    debug "Brr! It's cold"
    terminate 0

main :: IO ()
main = execute program

{-
main :: IO ()
main = do
    context <- configure $ do
        initLoggingToStash
        clearScreenBeforeRunning

    execute' context program
-}

