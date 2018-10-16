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

        -- not strictly necessary but sets up next spec item
        it "sub-programs can be run" $ do
            context <- configure None blank
            user <- subProgram context (getApplicationState)
            user `shouldBe` None

        it "unlifting from lifted IO works" $
            execute $ do
                user1 <- getApplicationState
                withContext $ \runProgram -> do
                    user1 `shouldBe` None
                    user2 <- runProgram getApplicationState -- unlift!
                    user2 `shouldBe` user1
