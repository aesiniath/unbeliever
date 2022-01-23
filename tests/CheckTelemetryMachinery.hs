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
import Data.Word (Word32)
import Network.Info (MAC (..))
import Test.Hspec hiding (context)

import Core.Program
import Core.System
import Core.Telemetry
import Core.Telemetry.Identifiers
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
            toHexNormal32 1 `shouldBe` "00000001"
            toHexNormal32 10 `shouldBe` "0000000a"
            toHexNormal32 17 `shouldBe` "00000011"
            toHexNormal32 255 `shouldBe` "000000ff"
            toHexNormal32 256 `shouldBe` "00000100"
            toHexNormal32 1024 `shouldBe` "00000400"
            toHexNormal32 4096 `shouldBe` "00001000"
            toHexNormal32 65536 `shouldBe` "00010000"
            toHexNormal32 0xdcba0000 `shouldBe` "dcba0000"
            toHexNormal32 (maxBound - 1) `shouldBe` "fffffffe"
            toHexNormal32 maxBound `shouldBe` "ffffffff"

            toHexReversed32 1 `shouldBe` "10000000"
            toHexReversed32 10 `shouldBe` "a0000000"
            toHexReversed32 17 `shouldBe` "11000000"
            toHexReversed32 255 `shouldBe` "ff000000"
            toHexReversed32 256 `shouldBe` "00100000"
            toHexReversed32 1024 `shouldBe` "00400000"
            toHexReversed32 4096 `shouldBe` "00010000"
            toHexReversed32 65536 `shouldBe` "00001000"
            toHexReversed32 0xdcba0000 `shouldBe` "0000abcd"
            toHexReversed32 (maxBound - 1) `shouldBe` "efffffff"
            toHexReversed32 maxBound `shouldBe` "ffffffff"

        it "converts Word64 to hexidecimal" $ do
            toHexNormal64 1 `shouldBe` "0000000000000001"
            toHexNormal64 10 `shouldBe` "000000000000000a"
            toHexNormal64 17 `shouldBe` "0000000000000011"
            toHexNormal64 255 `shouldBe` "00000000000000ff"
            toHexNormal64 256 `shouldBe` "0000000000000100"
            toHexNormal64 1024 `shouldBe` "0000000000000400"
            toHexNormal64 4096 `shouldBe` "0000000000001000"
            toHexNormal64 65536 `shouldBe` "0000000000010000"
            toHexNormal64 0xdcba000042000000 `shouldBe` "dcba000042000000"
            toHexNormal64 (maxBound - 1) `shouldBe` "fffffffffffffffe"
            toHexNormal64 maxBound `shouldBe` "ffffffffffffffff"

            toHexReversed64 1 `shouldBe` "1000000000000000"
            toHexReversed64 10 `shouldBe` "a000000000000000"
            toHexReversed64 17 `shouldBe` "1100000000000000"
            toHexReversed64 255 `shouldBe` "ff00000000000000"
            toHexReversed64 256 `shouldBe` "0010000000000000"
            toHexReversed64 1024 `shouldBe` "0040000000000000"
            toHexReversed64 4096 `shouldBe` "0001000000000000"
            toHexReversed64 65536 `shouldBe` "0000100000000000"
            toHexReversed64 0xdcba000042000000 `shouldBe` "000000240000abcd"
            toHexReversed64 (maxBound - 1) `shouldBe` "efffffffffffffff"
            toHexReversed64 maxBound `shouldBe` "ffffffffffffffff"

        it "formats timestamp as span identifier" $ do
            createIdentifierSpan (TimeStamp 1) 0 `shouldBe` Span "1000000000000000"
            createIdentifierSpan (TimeStamp (fromIntegral (maxBound :: Int32))) 0 `shouldBe` Span "fffffff700000000"
            createIdentifierSpan (TimeStamp (fromIntegral (maxBound :: Word32))) 0 `shouldBe` Span "ffffffff00000000"
            createIdentifierSpan (TimeStamp (fromIntegral (maxBound :: Word32)) + 1) 0 `shouldBe` Span "0000000010000000"
            createIdentifierSpan (TimeStamp 1642770757512438606) 0 `shouldBe` Span "e43ade8dc4b4cc61"
            createIdentifierSpan (TimeStamp 1642770757512438607) 0 `shouldBe` Span "f43ade8dc4b4cc61"

        it "formats timestamp and address as trace identifier" $ do
            createIdentifierTrace (TimeStamp 0) 0 (MAC 0 0 0 0 0 0) `shouldBe` Trace "00000000000000000000000000000000"
            createIdentifierTrace (TimeStamp 0x0fedcba987654321) 0x2468 (MAC 0x1a 0x2b 0x3c 0x4d 0x5e 0x6f)
                `shouldBe` Trace
                    ( mconcat
                        ( fmap
                            packRope
                            [ reverse "0fedcba987654321"
                            , "2468"
                            , "1a2b3c4d5e6f"
                            ]
                        )
                    )

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
