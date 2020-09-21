{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CheckBytesBehaviour where

import Core.Text.Bytes ()
import Core.Text.Utilities (byteChunk)
import qualified Data.ByteString.Char8 as C
import Test.Hspec

checkBytesBehaviour :: Spec
checkBytesBehaviour = do
  describe "Bytes data type" $ do
    it "chunks Bytes in 64 bit words" $
      let expected =
            [ C.pack "Hello Wo",
              C.pack "rld! Goo",
              C.pack "d Bye."
            ]
       in do
            byteChunk (C.pack "Hello World! Good Bye.") `shouldBe` expected
