{-# LANGUAGE OverloadedStrings #-}

import CheckArgumentsParsing (checkArgumentsParsing)
import CheckBytesBehaviour
import CheckContainerBehaviour
import CheckJsonWrapper
import CheckProgramMonad
import CheckRopeBehaviour (checkRopeBehaviour)
import Core.System
import Test.Hspec

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
