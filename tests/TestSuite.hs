{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CheckJsonWrapper
import CheckArgumentsParsing

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkJsonWrapper
    checkArgumentsParsing
