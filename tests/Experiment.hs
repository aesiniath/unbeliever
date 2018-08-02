{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--import Data.Text (Text)
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

program :: Program ()
program = do
    name <- getProgramName
    write stdout name

    setProgramName "hello"

    name <- getProgramName
    write stdout name

    write stdout (render k)

    liftIO $ do
        let x = encodeToUTF8 j
        print x

        let (Just y) = decodeFromUTF8 b
        print y
        print (encodeToUTF8 y)


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

