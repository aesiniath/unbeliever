{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CheckRopeBehaviour
import CheckBytesBehaviour
import CheckJsonWrapper
import CheckArgumentsParsing
import CheckProgramMonad

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkRopeBehaviour
    checkBytesBehaviour
    checkJsonWrapper
    checkArgumentsParsing
    checkProgramMonad
