{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CheckRopeBehaviour
import CheckJsonWrapper
import CheckArgumentsParsing

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkRopeBehaviour
    checkJsonWrapper
    checkArgumentsParsing
