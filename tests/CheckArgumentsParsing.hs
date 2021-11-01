{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckArgumentsParsing (
    checkArgumentsParsing,
    main,
) where

import Core.Data.Structures
import Core.Program.Arguments
import Core.System.Base
import Test.Hspec

main :: IO ()
main = do
    finally (hspec checkArgumentsParsing) (putStrLn ".")

options1 :: [Options]
options1 =
    [ Option "verbose" (Just 'v') Empty "Make the program verbose"
    , Option "quiet" (Just 'q') Empty "Be very very quiet, we're hunting wabbits"
    , Option "dry-run" Nothing (Value "WHEN") "Before trapping Road Runner, best to do a dry-run"
    ]

options2 :: [Options]
options2 =
    [ Option "recursive" Nothing Empty "Descend into darkness"
    , Argument "filename" "The file that you want"
    ]

options3 :: [Options]
options3 =
    [ Option "all" (Just 'a') Empty "Good will to everyone"
    ]

options4 :: [Options]
options4 =
    [ Remaining "All the rest of the files"
    ]

commands1 :: [Commands]
commands1 =
    [ Global
        options1
    , Command
        "add"
        "Add a new file"
        options2
    ]

commands2 :: [Commands]
commands2 =
    [ Global
        options1
    , Command
        "add"
        "Add a new file"
        options2
    , Command
        "commit"
        "Commit for eternity"
        options3
    ]

commands4 :: [Commands]
commands4 =
    [ Global
        options1
    , Command
        "add"
        "Add a new file"
        options2
    , Command
        "commit"
        "Commit for eternity"
        options4
    ]

checkArgumentsParsing :: Spec
checkArgumentsParsing = do
    describe "Parsing of simple command-lines" $ do
        it "recognizes a single specified options" $
            let config = simpleConfig options1
                actual = parseCommandLine config ["--verbose"]
                expect = Parameters Nothing (intoMap [("verbose", Empty)]) [] emptyMap
             in actual `shouldBe` Right expect
        it "recognizes all specified options" $
            let config = simpleConfig options1
                actual = parseCommandLine config ["--verbose", "--quiet", "--dry-run=Tomorrow"]
                expect =
                    Parameters
                        Nothing
                        ( intoMap
                            [ ("verbose", Empty)
                            , ("quiet", Empty)
                            , ("dry-run", Value "Tomorrow")
                            ]
                        )
                        []
                        emptyMap
             in actual `shouldBe` Right expect

        it "recognizes required arguments" $
            let config = simpleConfig options2
                actual = parseCommandLine config ["hello.txt"]
                expect =
                    Parameters
                        Nothing
                        ( intoMap
                            [ ("filename", Value "hello.txt")
                            ]
                        )
                        []
                        emptyMap
             in actual `shouldBe` Right expect

        it "handles valued parameter" $
            let config = simpleConfig options2
                actual = parseCommandLine config ["hello.txt"]
                expect =
                    Parameters
                        Nothing
                        ( intoMap
                            [ ("filename", Value "hello.txt")
                            ]
                        )
                        []
                        emptyMap
             in actual `shouldBe` Right expect

        it "rejects unknown options" $
            let config = simpleConfig options2
                actual = parseCommandLine config ["-a"]
             in actual `shouldBe` Left (UnknownOption "-a")

        it "rejects a malformed option" $
            let config = simpleConfig options2
                actual = parseCommandLine config ["-help"]
             in actual `shouldBe` Left (InvalidOption "-help")

        it "fails on missing argument" $
            let config = simpleConfig options2
                actual = parseCommandLine config []
             in actual `shouldBe` Left (MissingArgument "filename")

        it "accepts request for version" $
            let config = simpleConfig options1
                actual = parseCommandLine config ["--version"]
             in actual `shouldBe` Left VersionRequest

    describe "Parsing of complex command-lines" $ do
        it "recognizes only single command" $
            let config = complexConfig commands1
                actual = parseCommandLine config ["-q", "add", "--recursive", "Hello.hs"]
                expect =
                    Parameters
                        (Just "add")
                        ( intoMap
                            [ ("quiet", Empty)
                            , ("recursive", Empty)
                            , ("filename", Value "Hello.hs")
                            ]
                        )
                        []
                        emptyMap
             in actual `shouldBe` Right expect

        it "fails on missing command" $
            let config = complexConfig commands1
                actual = parseCommandLine config []
             in actual `shouldBe` Left (NoCommandFound)

        it "rejects an unknown command" $
            let config = complexConfig commands1
                actual = parseCommandLine config ["launch"]
             in actual `shouldBe` Left (UnknownCommand "launch")

        it "recognizes different command" $ -- ie, now from among multiple choices
            let config = complexConfig commands2
                actual = parseCommandLine config ["commit"]
                expect = Parameters (Just "commit") emptyMap [] emptyMap
             in actual `shouldBe` Right expect

        it "rejects further trailing arguments" $
            let config = complexConfig commands2
                actual = parseCommandLine config ["commit", "some"]
             in actual `shouldBe` Left (UnexpectedArguments ["some"])

        it "accepts trailing arguments as remainder" $
            let config = complexConfig commands4
                actual = parseCommandLine config ["commit", "one", "two", "tree"]
                expect = Parameters (Just "commit") emptyMap ["one", "two", "tree"] emptyMap
             in actual `shouldBe` Right expect

        -- in complex mode wasn't accpting --version as a global option.

        it "accepts request for version" $
            let config = complexConfig commands2
                actual = parseCommandLine config ["--version"]
             in actual `shouldBe` Left VersionRequest
