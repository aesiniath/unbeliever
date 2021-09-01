{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckContainerBehaviour where

import Core.Data.Structures
import Core.Text.Rope
import Test.Hspec

climbing :: [Int]
climbing = [1, 1, 2, 1, 2, 4, 1, 3, 9]

fibonacci :: [Int]
fibonacci = [1, 1, 2, 3, 5, 8, 13, 21]

introduction :: [(Int, Rope)]
introduction = [(2, " "), (3, "world"), (1, "hello")]

checkContainerBehaviour :: Spec
checkContainerBehaviour = do
    describe "Set data type" $ do
        it "calculates length accurately" $ do
            length fibonacci `shouldBe` 8
            let s = intoSet fibonacci
            length s `shouldBe` 7

        it "converts to list in Ord order" $ do
            let s = intoSet climbing
            length s `shouldBe` 5
            fromSet s `shouldBe` [1, 2, 3, 4, 9]

    describe "Map data type" $ do
        it "calculates length accurately" $ do
            length introduction `shouldBe` 3
            let p = intoMap introduction
            length p `shouldBe` 3

        it "values can be looked up" $ do
            let p = intoMap introduction
            containsKey 3 p `shouldBe` True
            lookupKeyValue 3 p `shouldBe` (Just "world")
            containsKey 4 p `shouldBe` False
            lookupKeyValue 4 p `shouldBe` Nothing

        it "values can be inserted into Map" $ do
            let p = intoMap introduction
            let p' = insertKeyValue 4 "!" p
            containsKey 4 p' `shouldBe` True
            lookupKeyValue 4 p' `shouldBe` (Just "!")

        it "converts to list in Ord order" $ do
            let p = intoMap introduction
            fromMap p `shouldBe` [(1, "hello"), (2, " "), (3, "world")]

        it "updated values supercede existing values" $ do
            let p = intoMap introduction
            let p' = insertKeyValue 2 "&" p
            containsKey 2 p' `shouldBe` True
            lookupKeyValue 2 p' `shouldBe` (Just "&")
