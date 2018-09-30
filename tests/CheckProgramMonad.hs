{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckProgramMonad where

import Test.Hspec

import Core.Program.Arguments
import Core.Program.Context

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
    describe "Config type" $ do
        it "Functor instance behaves" $
          let
            config = simple options
          in
            unConfig config `shouldBe` None
