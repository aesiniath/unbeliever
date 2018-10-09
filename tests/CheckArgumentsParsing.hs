{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckArgumentsParsing where

import Test.Hspec

import Core.Program.Arguments

options1 =
    [ Option "verbose" (Just 'v') "Make the program verbose"
    , Option "quiet" (Just 'q') "Be very very quiet, we're hunting wabbits"
    , Option "dry-run" Nothing "Before trapping Road Runner, best to do a dry-run"
    ]

options2 =
    [ Option "recursive" Nothing "Descend into darkness"
    , Argument "filename" "The file that you want"
    ]

options3 =
    [ Option "all" (Just 'a') "Good will to everyone"
    ]


commands1 =
    [ Global
        options1
    , Command "add" "Add a new file"
        options2
    ]

commands2 =
    [ Global
        options1
    , Command "add" "Add a new file"
        options2
    , Command "commit" "Commit for eternity"
        options3
    ]


checkArgumentsParsing :: Spec
checkArgumentsParsing = do
    describe "Parsing of simple command-lines" $ do
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
            actual `shouldBe` Left (UnknownOption "-a")

        it "rejects a malformed option" $
          let
            config = simple options2
            actual = parseCommandLine config ["-help"]
          in
            actual `shouldBe` Left (InvalidOption "-help")

        it "fails on missing argument" $
          let
            config = simple options2
            actual = parseCommandLine config []
          in
            actual `shouldBe` Left (MissingArgument "filename")

    describe "Parsing of complex command-lines" $ do

        it "recognizes only single command" $
          let
            config = complex commands1
            actual = parseCommandLine config ["-q", "add", "--recursive", "Hello.hs"]
            expect = Parameters (Just "add")
                [ ("quiet", Empty)
                , ("recursive", Empty)
                , ("filename", Value "Hello.hs")
                ] []
          in
            actual `shouldBe` Right expect

        it "fails on missing command" $
          let
            config = complex commands1
            actual = parseCommandLine config []
          in
            actual `shouldBe` Left (NoCommandFound)

        it "rejects an unknown command" $
          let
            config = complex commands1
            actual = parseCommandLine config ["launch"]
          in
            actual `shouldBe` Left (UnknownCommand "launch")

        it "recognizes different command" $ -- ie, now from among multiple choices
          let
            config = complex commands2
            actual = parseCommandLine config ["commit"]
            expect = Parameters (Just "commit") [] []
          in
            actual `shouldBe` Right expect


        it "rejects further trailing arguments" $
          let
            config = complex commands2
            actual = parseCommandLine config ["commit", "some"]
          in
            actual `shouldBe` Left (UnexpectedArguments ["some"])

