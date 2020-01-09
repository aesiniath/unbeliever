{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Core.System
import CheckRopeBehaviour (checkRopeBehaviour)
import CheckBytesBehaviour
import CheckContainerBehaviour
import CheckJsonWrapper
import CheckArgumentsParsing
import CheckProgramMonad

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkRopeBehaviour
    checkBytesBehaviour
    checkContainerBehaviour
    checkJsonWrapper
    checkArgumentsParsing
    checkProgramMonad
