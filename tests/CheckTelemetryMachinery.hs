{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTelemetryMachinery where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue)
import Core.Program.Execute (loopForever)
import Core.System
import qualified Control.Concurrent.Async as Async (async, wait)

import Test.Hspec hiding (context)
import Control.Concurrent (threadDelay)

countingAction :: Int -> [Int] -> IO ()
countingAction target ints = sum ints `shouldBe` target

matchingAction :: [Int] -> [Int] -> IO ()
matchingAction target items = items `shouldBe` target

store :: MVar [Int]
store = unsafePerformIO (newMVar [])

storingAction :: [Int] -> IO ()
storingAction items = do
    modifyMVar_ store (\value -> pure (value ++ items))

checkTelemetryMachinery :: Spec
checkTelemetryMachinery = do
    describe "Queue processing" $ do
        it "processes an item put on queue" $ do
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue (Just 1)
                writeTQueue queue Nothing

            loopForever (countingAction 1) queue

        it "processes mutlitple items" $ do
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue (Just 1)
                writeTQueue queue (Just 2)
                writeTQueue queue (Just 3)
                writeTQueue queue Nothing

            loopForever (matchingAction [1, 2, 3]) queue

        it "stops even if only empty" $ do
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue Nothing

            loopForever (countingAction 0) queue

        it "extended sequeence handled in right order" $ do
            queue <- newTQueueIO

            a <- Async.async (loopForever storingAction queue)

            mapM_
                ( \i -> atomically $ do
                    writeTQueue queue (Just i)
                )
                ([1 .. 100] :: [Int])
            threadDelay 100000
            mapM_
                ( \i -> atomically $ do
                    writeTQueue queue (Just i)
                )
                ([101 .. 200] :: [Int])
            threadDelay 100000
            mapM_
                ( \i -> atomically $ do
                    writeTQueue queue (Just i)
                )
                ([201 .. 300] :: [Int])


            atomically $ do
                writeTQueue queue Nothing

            Async.wait a

            value <- readMVar store
            value `shouldBe` ([1 .. 300] :: [Int])
