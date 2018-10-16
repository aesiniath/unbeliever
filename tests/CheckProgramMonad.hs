{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckProgramMonad where

import Test.Hspec

import Core.Program.Arguments
import Core.Program.Execute
import Core.Program.Unlift

options =
    [ Option "all" (Just 'a') "Good will to everyone"
    ]

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
