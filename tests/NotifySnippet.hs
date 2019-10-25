{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import qualified Data.ByteString.Char8 as C
import Control.Monad (forever)

import Core.Program
import Core.Text
import Core.System

main :: IO ()
main = execute $ do
    setVerbosityLevel Debug
    event "Starting..."

    forever (waitForChange ["tests/Snippet.hs"])

    write "Done"
