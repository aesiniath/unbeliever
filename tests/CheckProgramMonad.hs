{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module CheckProgramMonad where

import Test.Hspec hiding (context)

import Core.Program.Arguments
import Core.Program.Execute
import Core.Program.Unlift
import Core.System.Base

options :: [Options]
options =
    [ Option "all" (Just 'a') "Good will to everyone"
    ]

commands :: [Commands]
commands =
    [ Global
        options
    , Command "go-forth" "And multiply"
        []
    ]

checkProgramMonad :: Spec
checkProgramMonad = do
    describe "Context type" $ do
        it "Eq instance for None behaves" $
            None `shouldBe` None

    describe "Program monad" $ do
        it "execute with blank Context as expected" $ do
            context <- configure None blank
            executeWith context $ do
                user <- getApplicationState
                liftIO $ do
                    user `shouldBe` None

        it "execute with simple Context as expected" $ do
            context <- configure None (simple options)
            executeWith context $ do
                params <- getCommandLine
                liftIO $ do
                    -- this assumes that hspec isn't passing any
                    -- command-line arguments through to us.
                    params `shouldBe` (Parameters Nothing [] [])

        it "sub-programs can be run" $ do
            context <- configure None blank
            result <- subProgram context (getApplicationState)
            result `shouldBe` None
