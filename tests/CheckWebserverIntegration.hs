{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception qualified as Base (finally)
import Data.Trie qualified as Trie

import Test.Hspec hiding (context)

main :: IO ()
main = do
    Base.finally (hspec checkWebserverIntegration) (putStrLn ".")

{-
type Application =
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
-}

checkWebserverIntegration :: Spec
checkWebserverIntegration = do
    {-
    dummyApplication :: Application
    dummyApplication = undefined

        describe "Warp/WAI integration" $ do
            it "instantiates a webserver on a known port" $ do
                context <- configure "1" None blankConfig
                executeWith context $ do
                    launchWebserver 54321 dummyApplication
                    pure ()

    -}

    describe "Router routes routes" $ do
        it "Data.Trie match behaviour is understood" $ do
            let tree0 = Trie.singleton "/" (1 :: Int)
            let tree1 = Trie.insert "/status" (2 :: Int) tree0
            let tree2 = Trie.insert "/status/bump" (3 :: Int) tree1

            Trie.match tree2 "/"
                `shouldBe` Just ("/", 1, "")

            Trie.match tree2 "/notfound"
                `shouldBe` Just ("/", 1, "notfound")

            Trie.match tree2 "/status"
                `shouldBe` Just ("/status", 2, "")

            Trie.match tree2 "/status/RABBIT/count"
                `shouldBe` Just ("/status", 2, "/RABBIT/count")

            Trie.match tree2 "/status/bu"
                `shouldBe` Just ("/status", 2, "/bu")

            Trie.match tree2 "/status/bump"
                `shouldBe` Just ("/status/bump", 3, "")

            Trie.submap "/status" tree2
                `shouldBe` Trie.fromList [("/status", 2), ("/status/bump", 3)]

            Trie.lookup "/status" tree2
                `shouldBe` Just 2

            Trie.lookup "/status/bu" tree2
                `shouldBe` Nothing
