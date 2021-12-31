{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Core.Program
import Core.System
import Core.Text
import qualified Data.ByteString.Char8 as C

b = intoBytes (C.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

main :: IO ()
main = execute $ do
    info "Processing..."
    debugR "b" b

    let x = error "No!"

    write $ case x of
        Nothing -> "Nothing!"

    sleepThread 0.2

    write "Done"
