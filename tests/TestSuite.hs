{-# LANGUAGE OverloadedStrings #-}

import CheckArgumentsParsing (checkArgumentsParsing)
import CheckBytesBehaviour
import CheckContainerBehaviour
import CheckExternalizing (checkExternalizing)
import CheckJsonWrapper
import CheckProgramMonad
import CheckRopeBehaviour (checkRopeBehaviour)
import CheckTelemetryMachinery
import CheckTimeStamp
import CheckWebserverIntegration (checkWebserverIntegration)
import Control.Exception (finally)
import Test.Hspec

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkRopeBehaviour
    checkBytesBehaviour
    checkContainerBehaviour
    checkTimeStamp
    checkExternalizing
    checkJsonWrapper
    checkArgumentsParsing
    checkProgramMonad
    checkTelemetryMachinery
    checkWebserverIntegration
