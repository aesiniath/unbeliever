{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CheckRopeBehaviour
import CheckBytesBehaviour
import CheckContainerBehaviour
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
    checkContainerBehaviour
    checkJsonWrapper
    checkArgumentsParsing
    checkProgramMonad
