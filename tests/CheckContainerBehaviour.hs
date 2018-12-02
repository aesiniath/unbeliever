{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckContainerBehaviour where

import qualified Data.List as List
import Test.Hspec

import Core.Data.Structures
import Core.Text.Rope

climbing :: [Int]
climbing = [1,1,2,1,2,4,1,3,9]

fibonacci :: [Int]
fibonacci = [1,1,2,3,5,8,13,21]

checkContainerBehaviour :: Spec
checkContainerBehaviour = do
    describe "Set data type" $
      do
        it "calculates length accurately" $ do
            length fibonacci `shouldBe` 8
            let s = intoSet fibonacci
            length s `shouldBe` 7

        it "converts to list in Ord order" $ do
            let s = intoSet climbing
            length s `shouldBe` 5
            fromSet s `shouldBe` [1,2,3,4,9]
