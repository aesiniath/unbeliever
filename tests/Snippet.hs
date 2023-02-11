{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Control.Monad (forM_)
import Core.Program
import Core.System
import Core.Text
import Data.ByteString.Char8 qualified as C

main :: IO ()
main = execute $ do
    info "Begin"
    forkThread $ do
        forM_ [1 :: Int ..] $ \i -> do
            debugS "value" i

    warn "Executing new process"
    execExternalProcess ["/bin/echo", "Hello World"]
