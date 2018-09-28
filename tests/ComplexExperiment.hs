{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Text.Heredoc

import Core.Text
import Core.Program
import Core.System

program :: Program ()
program = do
    event "Starting..."

    params <- getCommandLine
    debugS "params" params

    event "Done."
    terminate 0


main :: IO ()
main = do
    context <- configure (complex
        [ Global
            [ Option "verbose" (Just 'v') [here|
                Turn on event level logging to console.
                Valid values are "event", "debug", and "none" (the default
                if you don't specify the verbose option).
              |]
            , Option "logging-and-cutting" Nothing [here|
                Valid values are "console", "file:/path/to/file.log", and "syslog".
              |]
            , Option "quiet" (Just 'q') [here|
                Supress normal output.
              |]
            ]
        , Command "add" "Add a file."
            [ Argument "filename" "File to add."
            ]

        , Command "commit" "Commit your changes to the repository."
            [ Option "message" (Just 'm') "Specify commit message (instead of using editor)."
            ]

        , Command "launch" "Fire the weapons at the alien horde."
            [ Option "all" (Just 'a') "Target all the baddies."
            , Option "other" Nothing "Another option."
            , Argument "input-file" [here|
                The file you want to read the launch codes from.
              |]
            , Argument "main-output-device" [here|
                The device you want to draw the pretty picture to.
              |]
            ]
  
        , Environment
            [ Variable "CRAZY_MODE" "Specify how many crazies to activate."
            , Variable "GITHUB_TOKEN" "OAuth token to access GitHub."
            ]
        ])
  
    executeWith context program

