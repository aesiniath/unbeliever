{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

import Gauge.Main
import GHC.Conc

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as U
import qualified Data.Text.Lazy.Builder as U
import Data.String.Here
import System.IO

import Core.Text.Rope

main :: IO ()
main = do
    GHC.Conc.setNumCapabilities 4
    defaultMain
       [bench "data-text" (nfIO (runTextToFile complexText)),
        bench "core-rope" (nfIO (runRopeToFile complexRope))]
    putStrLn "Complete."

runTextToFile :: U.Builder -> IO ()
runTextToFile build = do 
    withFile "/tmp/garbage-text.txt" WriteMode $ \handle -> do
        T.hPutStr handle (U.toStrict (U.toLazyText build))

runRopeToFile :: Rope -> IO ()
runRopeToFile text = do 
    withFile "/tmp/garbage-rope.txt" WriteMode $ \handle -> do
        hOutput handle text

----

complexText :: U.Builder
complexText =
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
    [here|
Opinionated Haskell Interoperability

Copyright © 2018 Operational Dynamics Consulting, Pty Ltd and Others
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above
       copyright notice, this list of conditions and the following
       disclaimer in the documentation and/or other materials provided
       with the distribution.
      
    3. Neither the name of the project nor the names of its contributors
       may be used to endorse or promote products derived from this 
       software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    |]

complexRope :: Rope
complexRope =
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
    [here|
Opinionated Haskell Interoperability

Copyright © 2018 Operational Dynamics Consulting, Pty Ltd and Others
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above
       copyright notice, this list of conditions and the following
       disclaimer in the documentation and/or other materials provided
       with the distribution.
      
    3. Neither the name of the project nor the names of its contributors
       may be used to endorse or promote products derived from this 
       software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    |]
