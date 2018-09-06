{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckArgumentsParsing where

import Test.Hspec

import Core.Text
import Core.Program.Arguments

options =
    [ Option "verbose" (Just 'v') "Make the program verbose"
    , Option "quiet" (Just 'q') "Be very very quiet, we're hunting wabbits"
    , Option "dry-run" Nothing "Before trapping Road Runner, best to do a dry-run"
    ]



checkArgumentsParsing :: Spec
checkArgumentsParsing = do
    describe "Parsing of command-line arguments" $ do
        it "recognizes a single specified options" $
          let
            config = simple options
            actual = parseCommandLine config ["--verbose"]
            expect = Parameters Nothing [("verbose", ParameterValue "")] []
          in
            actual `shouldBe` expect
        it "recognizes all specified options" $
          let
            config = simple options
            actual = parseCommandLine config ["--verbose --quiet --dry-run"]
            expect = Parameters Nothing
              [ ("verbose", ParameterValue "")
              , ("quiet", ParameterValue "")
              , ("dry-run", ParameterValue "")
              ] []
          in
            actual `shouldBe` expect
