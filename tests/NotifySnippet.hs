{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Control.Monad (forever)
import Core.Program
import Core.System
import Core.Text
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = execute $ do
  setVerbosityLevel Debug
  event "Starting..."

  forever (waitForChange ["tests/Snippet.hs"])

  write "Done"
