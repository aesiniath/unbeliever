{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckRopeBehaviour (
    checkRopeBehaviour,
    main,
) where

import Core.System (finally)
import Core.Text.Rope
import Core.Text.Utilities
import Data.Char (isSpace)
import qualified Data.FingerTree as F
import Data.Hashable (hash)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as U
import qualified Data.Text.Short as S
import Test.Hspec

main :: IO ()
main = do
    finally (hspec checkRopeBehaviour) (putStrLn ".")

hydrogen = "H₂" :: Rope

sulfate = "SO₄" :: Rope

sulfuric_acid = hydrogen <> sulfate

compound = "3" <> "-" <> "ethyl" <> "-" <> "4" <> "-" <> "methyl" <> "hexane" :: Rope

checkRopeBehaviour :: Spec
checkRopeBehaviour = do
    describe "Rope data type" $ do
        it "knows what a singleton is" $ do
            singletonRope 'i' `shouldBe` "i"

        it "IsString instance behaves" $ do
            unRope ("Hello" :: Rope) `shouldBe` F.singleton (S.pack "Hello")

        it "calculates length accurately" $ do
            widthRope hydrogen `shouldBe` 2
            widthRope sulfate `shouldBe` 3
            widthRope (hydrogen <> sulfate) `shouldBe` 5

        it "Eq instance behaves" $ do
            ("" :: Rope) == ("" :: Rope) `shouldBe` True
            ("C" :: Rope) /= ("" :: Rope) `shouldBe` True
            ("" :: Rope) /= ("F" :: Rope) `shouldBe` True
            ("O" :: Rope) == ("O" :: Rope) `shouldBe` True
            ("H₂" :: Rope) == ("H₂" :: Rope) `shouldBe` True
            ("H₂" :: Rope) /= ("SO₄" :: Rope) `shouldBe` True

        it "Hashable instance behaves" $ do
            hash ("Hello" :: Rope) `shouldBe` hash (singletonRope 'H' <> intoRope ("ello" :: String))

        -- depended on Textual instance for String being fixed and
        -- the Eq instance being customized to ignore tree structure
        it "concatonates two Ropes correctly (Monoid)" $ do
            ("H₂" :: Rope) <> ("SO₄" :: Rope) `shouldBe` ("H₂SO₄" :: Rope)

        it "concatonates two Ropes correctly (Textual)" $ do
            appendRope ("SO₄" :: Rope) ("H₂" :: Rope) `shouldBe` ("H₂SO₄" :: Rope)

        it "replicates itself" $ do
            replicateRope 3 "hello" `shouldBe` ("hellohellohello" :: Rope)
            length (unRope (replicateRope 3 "hello")) `shouldBe` 3
            replicateRope 3 "" `shouldBe` emptyRope
            replicateRope 0 "hello" `shouldBe` emptyRope
            replicateChar 3 'x' `shouldBe` ("xxx" :: Rope)
            replicateChar 0 'x' `shouldBe` ("" :: Rope)

        it "exports to ByteString" $
            let expected = T.encodeUtf8 (T.pack "H₂SO₄")
             in do
                    fromRope sulfuric_acid `shouldBe` expected

        it "exports to Text (Strict)" $ do
            fromRope sulfuric_acid `shouldBe` T.pack "H₂SO₄"

        it "exports to Text (Lazy)" $ do
            fromRope sulfuric_acid `shouldBe` U.pack "H₂SO₄"

        it "does the splits" $ do
            -- compare behaviour on Haskell lists
            List.splitAt 0 ("123456789" :: String) `shouldBe` ("", "123456789")
            List.splitAt 3 ("123456789" :: String) `shouldBe` ("123", "456789")

            -- expect same behaviour of Rope
            splitRope 0 ("123456789" :: Rope) `shouldBe` ("", "123456789")
            splitRope 3 ("123456789" :: Rope) `shouldBe` ("123", "456789")
            splitRope 9 ("123456789" :: Rope) `shouldBe` ("123456789", "")
            splitRope 10 ("123456789" :: Rope) `shouldBe` ("123456789", "")
            splitRope (-1) ("123456789" :: Rope) `shouldBe` ("", "123456789")

            -- exercise splitRopeting at and between piece boundaries
            splitRope 0 compound `shouldBe` ("", "3-ethyl-4-methylhexane")
            splitRope 1 compound `shouldBe` ("3", "-ethyl-4-methylhexane")
            splitRope 2 compound `shouldBe` ("3-", "ethyl-4-methylhexane")
            splitRope 4 compound `shouldBe` ("3-et", "hyl-4-methylhexane")
            --                             1234567890
            splitRope 10 compound `shouldBe` ("3-ethyl-4-", "methylhexane")
            splitRope 11 compound `shouldBe` ("3-ethyl-4-m", "ethylhexane")
            splitRope 16 compound `shouldBe` ("3-ethyl-4-methyl", "hexane")
            splitRope 21 compound `shouldBe` ("3-ethyl-4-methylhexan", "e")
            widthRope compound `shouldBe` 22
            splitRope 22 compound `shouldBe` ("3-ethyl-4-methylhexane", "")
            splitRope 23 compound `shouldBe` ("3-ethyl-4-methylhexane", "")
            splitRope (-1) compound `shouldBe` ("", "3-ethyl-4-methylhexane")

        it "does insertion correctly" $ do
            insertRope 3 "two" "onethree" `shouldBe` "onetwothree"
            insertRope 3 "Con" "Def 1" `shouldBe` "DefCon 1"
            insertRope 0 "one" "twothree" `shouldBe` "onetwothree"
            insertRope 6 "three" "onetwo" `shouldBe` "onetwothree"

        it "finds characters correctly" $ do
            findIndexRope (== '3') compound `shouldBe` (Just 0)
            findIndexRope (== '4') compound `shouldBe` (Just 8)
            findIndexRope (== '!') compound `shouldBe` Nothing
            findIndexRope (== 'e') compound `shouldBe` (Just 2)

    describe "QuasiQuoted string literals" $ do
        it "string literal is IsString" $ do
            [quote|Hello|] `shouldBe` ("Hello" :: String)
            [quote|Hello|] `shouldBe` ("Hello" :: Rope)

        it "trims multi-line string literal" $ do
            [quote|
Hello
            |]
                `shouldBe` ("Hello\n" :: Rope)
            [quote|
Hello
World
            |]
                `shouldBe` ("Hello\nWorld\n" :: Rope)

    describe "Splitting into words" $ do
        it "breaks short text into chunks" $ do
            intoChunks isSpace "" `shouldBe` []
            intoChunks isSpace "Hello" `shouldBe` ["Hello"]
            intoChunks isSpace "Hello World" `shouldBe` ["Hello", "World"]
            intoChunks isSpace "Hello " `shouldBe` ["Hello", ""]
            intoChunks isSpace " Hello" `shouldBe` ["", "Hello"]
            intoChunks isSpace " Hello " `shouldBe` ["", "Hello", ""]

        it "breaks consecutive short texts into chunks" $ do
            intoPieces isSpace "Hello" (Nothing, [])
                `shouldBe` (Just "Hello", [])
            intoPieces isSpace "" (Nothing, [])
                `shouldBe` (Nothing, [])
            intoPieces isSpace "" (Nothing, ["World"])
                `shouldBe` (Nothing, ["World"])
            intoPieces isSpace "This is" (Nothing, ["", "a", "", "test."])
                `shouldBe` (Just "This", ["is", "", "a", "", "test."])
            intoPieces isSpace "This i" (Just "s", ["", "a", "", "test."])
                `shouldBe` (Just "This", ["is", "", "a", "", "test."])

        it "single piece containing multiple words splits correctly" $
            let text = "This is a test"
             in do
                    breakWords text `shouldBe` ["This", "is", "a", "test"]

        it "single piece, long run of whitespace splits correctly" $
            let text = "This is\na    test"
             in do
                    breakWords text `shouldBe` ["This", "is", "a", "test"]

        it "text spanning two pieces can be split into words" $
            let text = "This is " <> "a test"
             in do
                    breakWords text `shouldBe` ["This", "is", "a", "test"]

        it "text spanning many pieces can be split into words" $
            let text = "st" <> "" <> "op" <> "" <> " " <> " " <> "and go" <> "op"
             in do
                    breakWords text `shouldBe` ["stop", "and", "goop"]

        it "empty and whitespace-only corner cases handled correctly" $
            let text = "  " <> "" <> "stop" <> "" <> "  "
             in do
                    breakWords text `shouldBe` ["stop"]

    describe "Splitting into lines" $ do
        it "preconditions are met" $ do
            breakLines "" `shouldBe` []
            breakLines "Hello" `shouldBe` ["Hello"]
            breakLines "Hello\nWorld" `shouldBe` ["Hello", "World"]
            breakLines "Hello\n" `shouldBe` ["Hello"]
            breakLines "\nHello" `shouldBe` ["", "Hello"]
            breakLines "\nHello\n" `shouldBe` ["", "Hello"]
            breakLines "Hello\nWorld\n" `shouldBe` ["Hello", "World"]
            breakLines "Hello\n\nWorld\n" `shouldBe` ["Hello", "", "World"]
            breakLines "Hello\n\nWorld\n\n" `shouldBe` ["Hello", "", "World", ""]

        it "single piece containing multiple lines splits correctly" $
            let para =
                    [quote|
This is a test
of the Emergency
Broadcast
System, beeeeep
|]
             in do
                    breakLines para
                        `shouldBe` [ "This is a test"
                                   , "of the Emergency"
                                   , "Broadcast"
                                   , "System, beeeeep"
                                   ]

        it "preserves blank lines" $
            let para =
                    [quote|
First line.

Third line.
|]
             in do
                    breakLines para
                        `shouldBe` [ "First line."
                                   , ""
                                   , "Third line."
                                   ]

    describe "Formatting paragraphs" $ do
        it "multi-line paragraph rewraps correctly" $
            let para =
                    [quote|
Hello this is
a test
 of the Emergency Broadcast System
            |]
             in wrap 20 para
                    `shouldBe` [quote|
Hello this is a test
of the Emergency
Broadcast System|]

    describe "Lines and columns" $ do
        it "calculate position of a given block" $ do
            calculatePositionEnd "" `shouldBe` (1, 1)
            calculatePositionEnd "Hello" `shouldBe` (1, 6)
            calculatePositionEnd "Hello\nWorld" `shouldBe` (2, 6)
            calculatePositionEnd "\nWorld" `shouldBe` (2, 6)
            calculatePositionEnd "\n" `shouldBe` (2, 1)
