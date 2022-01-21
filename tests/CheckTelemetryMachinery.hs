{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTelemetryMachinery where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async (async, wait)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.UUID (fromString, fromWords, nil)
import Data.Word (Word32)
import Test.Hspec hiding (context)

import Core.Program
import Core.System
import Core.Telemetry
import Core.Text

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
    describe "Trace and Span identifiers" $ do
        it "converts Word32 to hexidecimal" $ do
            toHexNormal 1 `shouldBe` "00000001"
            toHexNormal 10 `shouldBe` "0000000a"
            toHexNormal 17 `shouldBe` "00000011"
            toHexNormal 255 `shouldBe` "000000ff"
            toHexNormal 256 `shouldBe` "00000100"
            toHexNormal 1024 `shouldBe` "00000400"
            toHexNormal 4096 `shouldBe` "00001000"
            toHexNormal 65536 `shouldBe` "00010000"
            toHexNormal (maxBound - 1) `shouldBe` "fffffffe"
            toHexNormal maxBound `shouldBe` "ffffffff"

            toHexReversed 1 `shouldBe` "10000000"
            toHexReversed 10 `shouldBe` "a0000000"
            toHexReversed 17 `shouldBe` "11000000"
            toHexReversed 255 `shouldBe` "ff000000"
            toHexReversed 256 `shouldBe` "00100000"
            toHexReversed 1024 `shouldBe` "00400000"
            toHexReversed 4096 `shouldBe` "00010000"
            toHexReversed 65536 `shouldBe` "00001000"
            toHexReversed (maxBound - 1) `shouldBe` "efffffff"
            toHexReversed maxBound `shouldBe` "ffffffff"

        it "formats timestamp as span identifier" $ do
            convertToSpan32 (TimeStamp 1) `shouldBe` "1000000000000000"
            convertToSpan32 (TimeStamp (fromIntegral (maxBound :: Int32))) `shouldBe` "fffffff700000000"
            convertToSpan32 (TimeStamp (fromIntegral (maxBound :: Word32))) `shouldBe` "ffffffff00000000"
            convertToSpan32 (TimeStamp (fromIntegral (maxBound :: Word32)) + 1) `shouldBe` "0000000010000000"
            convertToSpan32 (TimeStamp 1642770757512438606) `shouldBe` "e43ade8dc4b4cc61"
            convertToSpan32 (TimeStamp 1642770757512438607) `shouldBe` "f43ade8dc4b4cc61"

        it "formats UUID as trace identifier" $ do
            convertToTrace64 nil `shouldBe` "00000000000000000000000000000000"
            let uuid1 = fromWords 2 0 0 1
            convertToTrace64 uuid1 `shouldBe` "20000000000000000000000000000001"
            let uuid2 = fromMaybe nil (fromString "3f648e86-9642-4af6-b8ff-bd2c64c6eedd")
            convertToTrace64 uuid2 `shouldBe` packRope ("68e846f3" ++ "6fa42469" ++ "b8ff" ++ "bd2c64c6eedd")

    describe "Queue processing" $ do
        it "processes an item put on queue" $ do
            v <- newMVar Debug
            out <- newTQueueIO
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue (Just 1)
                writeTQueue queue Nothing

            loopForever (countingAction 1) v out queue

        it "processes mutlitple items" $ do
            v <- newMVar Debug
            out <- newTQueueIO
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue (Just 1)
                writeTQueue queue (Just 2)
                writeTQueue queue (Just 3)
                writeTQueue queue Nothing

            loopForever (matchingAction [1, 2, 3]) v out queue

        it "stops even if only empty" $ do
            v <- newMVar Debug
            out <- newTQueueIO
            queue <- newTQueueIO

            atomically $ do
                writeTQueue queue Nothing

            loopForever (countingAction 0) v out queue

        it "extended sequence handled in right order" $ do
            v <- newMVar Debug
            out <- newTQueueIO
            queue <- newTQueueIO

            a <- Async.async (loopForever storingAction v out queue)

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
