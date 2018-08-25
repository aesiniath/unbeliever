{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import CheckJsonWrapper

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkJsonWrapper
