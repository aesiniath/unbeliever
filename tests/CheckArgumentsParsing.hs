{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckArgumentsParsing where

import Test.Hspec
import Control.Exception.Safe (throw)

import Core.Text
import Core.Program.Arguments

options1 =
    [ Option "verbose" (Just 'v') "Make the program verbose"
    , Option "quiet" (Just 'q') "Be very very quiet, we're hunting wabbits"
    , Option "dry-run" Nothing "Before trapping Road Runner, best to do a dry-run"
    ]

options2 =
    [ Argument "filename" "The file that you want"
    ]


checkArgumentsParsing :: Spec
checkArgumentsParsing = do
    describe "Parsing of command-line arguments" $ do
        it "recognizes a single specified options" $
          let
            config = simple options1
            actual = parseCommandLine config ["--verbose"]
            expect = Parameters Nothing [("verbose", Empty)] []
          in
            actual `shouldBe` Right expect
        it "recognizes all specified options" $
          let
            config = simple options1
            actual = parseCommandLine config ["--verbose", "--quiet", "--dry-run=Tomorrow"]
            expect = Parameters Nothing
              [ ("verbose", Empty)
              , ("quiet", Empty)
              , ("dry-run", Value "Tomorrow")
              ] []
          in
            actual `shouldBe` Right expect
        it "recognizes required arguments" $
          let
            config = simple options2
            actual = parseCommandLine config ["hello.txt"]
            expect = Parameters Nothing
              [ ("filename", Value "hello.txt")
              ] []
          in
            actual `shouldBe` Right expect
        it "rejects unknown options" $
          let
            config = simple options2
            actual = parseCommandLine config ["-a"]
          in
            case actual of
                Left (UnknownOption _)  -> passed
                _                       -> failed

        it "rejects a malformed option" $
          let
            config = simple options2
            actual = parseCommandLine config ["-help"]
          in
            case actual of
                Left (InvalidOption _)  -> passed
                _                       -> failed

{-
    Clearly I'm missing something obvious, but this is the best way I could
    think of to make sure the expressiong at the end of the above do block had
    the correct type.
-}

passed :: Expectation
passed = True `shouldBe` True

failed :: Expectation
failed = False `shouldBe` True
