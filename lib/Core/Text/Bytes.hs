{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}        -- FIXME
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- FIXME

module Core.Text.Bytes
    ( Bytes(..)
    , fromBytes
    , intoBytes
    ) where

import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)

data Bytes
    = StrictBytes S.ByteString
    | LazyBytes L.ByteString
    | ListBytes [Word8]
    deriving (Show, Eq)

{-|
Conversion to and from various types containing binary data into our
convenience Bytes type.
-}
class Binary a where
    fromBytes :: Bytes -> a
    intoBytes :: a -> Bytes

instance Binary S.ByteString where
    fromBytes (StrictBytes b') = b'
    intoBytes b' = StrictBytes b'

instance Binary L.ByteString where
    fromBytes (StrictBytes b') = L.fromStrict b'
    intoBytes b' = StrictBytes (L.toStrict b')      -- expensive


{-
instance Show Bytes where
    show x = case x of
        StrictBytes b' -> 
-}
