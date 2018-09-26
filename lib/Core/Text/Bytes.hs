{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Text.Bytes
    ( Bytes(..)
    , fromBytes
    , intoBytes
    ) where

import qualified Data.ByteString as S (ByteString, unpack, empty, append)
import qualified Data.ByteString.Char8 as C (elem)
import qualified Data.ByteString.Lazy as L (ByteString, unpack, fromStrict, toStrict)
import Data.String (IsString(..))
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as U (Text, toStrict, unpack)
import qualified Data.Text.Lazy.Encoding as U (decodeUtf8, encodeUtf8)

--import qualified Data.Text.IO as T
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
