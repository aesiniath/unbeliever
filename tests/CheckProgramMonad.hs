{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckProgramMonad where

import qualified Control.Exception.Safe as Safe
import Test.Hspec hiding (context)

import Core.Data.Structures
import Core.Program.Arguments
import Core.Program.Execute
import Core.Program.Unlift
import Core.System.Base

options :: [Options]
options =
    [ Option "all" (Just 'a') Empty "Good will to everyone"
    ]

commands :: [Commands]
commands =
    [ Global
        options
    , Command "go-forth" "And multiply"
        []
    ]

data Boom = Boom
    deriving Show

instance Exception Boom

boom :: Selector Boom
boom = const True

checkProgramMonad :: Spec
checkProgramMonad = do
    describe "Context type" $ do
        it "Eq instance for None behaves" $ do
            None `shouldBe` None

    describe "Program monad" $ do
        it "execute with blank Context as expected" $ do
            context <- configure "0.1" None blank
            executeWith context $ do
                user <- getApplicationState
                liftIO $ do
                    user `shouldBe` None

        it "execute with simple Context as expected" $ do
            context <- configure "0.1" None (simple options)
            executeWith context $ do
                params <- getCommandLine
                liftIO $ do
                    -- this assumes that hspec isn't passing any
                    -- command-line arguments through to us.
                    params `shouldBe` (Parameters Nothing emptyMap emptyMap)

        -- not strictly necessary but sets up next spec item
        it "sub-programs can be run" $ do
            context <- configure "0.1" None blank
            user <- subProgram context (getApplicationState)
            user `shouldBe` None

        it "unlifting from lifted IO works" $ do
            execute $ do
                user1 <- getApplicationState
                withContext $ \runProgram -> do
                    user1 `shouldBe` None
                    user2 <- runProgram getApplicationState -- unlift!
                    user2 `shouldBe` user1

        it "thrown Exceptions can be caught" $ do
            context <- configure "0.1" None blank
            (subProgram context (throw Boom)) `shouldThrow` boom

            -- ok, so with that established, now try **safe-exceptions**'s
            -- code. Note if we move the exception handling code from
            -- `execute` to `subProgram` this will have to adapt.
            Safe.catch
                (subProgram context (throw Boom))
                (\(_ :: Boom) -> return ())

        it "MonadThrow and MonadCatch behave" $ do
            context <- configure "0.1" None blank
            subProgram context $ do
                Safe.catch (Safe.throw Boom) (\(_ :: Boom) -> return ())
