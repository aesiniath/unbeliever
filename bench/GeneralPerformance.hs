{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

import Gauge.Main
import GHC.Conc

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as U
import qualified Data.Text.Lazy.Builder as U
import System.IO

import Core.Text.Rope

main :: IO ()
main = do
    b' <- B.readFile "LICENCE"
    let bodyText = T.decodeUtf8 b'
    let bodyRope = intoRope b'

    GHC.Conc.setNumCapabilities 4
    defaultMain
        [ bgroup "to-file"
            [ bench "data-text" (nfIO (runTextToFile (httpResponseText bodyText)))
            , bench "core-rope" (nfIO (runRopeToFile (httpResponseRope bodyRope)))
            ]
        , bgroup "convert-to-rope"
            [ bench "original" (nf (complexMess intoRope) b')
            , bench "experiment" (nf (complexMess unsafeIntoRope) b')
            ]
        ]
    putStrLn "Complete."

----

runTextToFile :: U.Builder -> IO ()
runTextToFile build = do 
    withFile "/tmp/garbage-text.txt" WriteMode $ \handle -> do
        T.hPutStr handle (U.toStrict (U.toLazyText build))

runRopeToFile :: Rope -> IO ()
runRopeToFile text = do 
    withFile "/tmp/garbage-rope.txt" WriteMode $ \handle -> do
        hOutput handle text

----

httpResponseText :: T.Text -> U.Builder
httpResponseText body =
    "HTTP/1.1" <> " " <> "200 OK" <> "\r\n" <>
    "Cache-Control" <> ": " <> "no-cache, must-revalidate" <> "\r\n" <>
    "Connection" <> ": " <> "keep-alive" <> "\r\n" <>
    "Content-Length" <> ": " <> "1609" <> "\r\n" <>
    "Content-Type" <> ": " <> "text/plain; charset=utf-8" <> "\r\n" <>
    "Date" <> ": " <> "Sun, 23 Sep 2018 09:16:05 GMT" <> "\r\n" <>
    "Expires" <> ": " <> "Thu, 01 Jan 1970 05:05:05 GMT" <> "\r\n" <>
    "Server" <> ": " <> "nginx/1.9.15" <> "\r\n" <>
    "Vary" <> ": " <> "Accept, Accept-Language" <> "\r\n" <>
    "\r\n" <>
    U.fromText body

httpResponseRope :: Rope -> Rope
httpResponseRope body =
    "HTTP/1.1" <> " " <> "200 OK" <> "\r\n" <>
    "Cache-Control" <> ": " <> "no-cache, must-revalidate" <> "\r\n" <>
    "Connection" <> ": " <> "keep-alive" <> "\r\n" <>
    "Content-Length" <> ": " <> "1609" <> "\r\n" <>
    "Content-Type" <> ": " <> "text/plain; charset=utf-8" <> "\r\n" <>
    "Date" <> ": " <> "Sun, 23 Sep 2018 09:16:05 GMT" <> "\r\n" <>
    "Expires" <> ": " <> "Thu, 01 Jan 1970 05:05:05 GMT" <> "\r\n" <>
    "Server" <> ": " <> "nginx/1.9.15" <> "\r\n" <>
    "Vary" <> ": " <> "Accept, Accept-Language" <> "\r\n" <>
    "\r\n" <>
    body

complexMess :: (B.ByteString -> Rope) -> B.ByteString -> Rope
complexMess f body =
    "HTTP/1.1" <> " " <> "200 OK" <> "\n" <>
    "Cache-Control" <> ": " <> "no-cache, must-revalidate" <> "\n" <>
    "Connection" <> ": " <> "keep-alive" <> "\n" <>
    "Content-Length" <> ": " <> "1609" <> "\n" <>
    "Content-Type" <> ": " <> "text/plain; charset=utf-8" <> "\n" <>
    "Date" <> ": " <> "Sun, 23 Sep 2018 09:16:05 GMT" <> "\n" <>
    "Expires" <> ": " <> "Thu, 01 Jan 1970 05:05:05 GMT" <> "\n" <>
    "Server" <> ": " <> "nginx/1.9.15" <> "\n" <>
    "Vary" <> ": " <> "Accept, Accept-Language" <> "\n" <>
    "\n" <>
    f body
