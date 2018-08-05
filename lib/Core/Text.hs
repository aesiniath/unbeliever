{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Text
    ( Text(..)
    , Bytes(..)
    , fromBytes
    , Textual(..)
    ) where

import qualified Data.ByteString as S (ByteString, unpack, empty, append)
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

data Text
    = UTF8 S.ByteString
    deriving (Eq, Read, Show, Generic)

instance Hashable Text

instance IsString Text where
    fromString = UTF8 . T.encodeUtf8 . T.pack

instance Monoid Text where
    mempty = UTF8 S.empty
    mappend (UTF8 b1') (UTF8 b2') = UTF8 (S.append b1' b2')

--  fromString :: IsString a => String -> a
--  fromTextual :: Text -> a

--
-- | Machinery to interpret a type as containing valid UTF-8 that can be
-- represented as a Text object.
--
class Textual a where
    fromText :: Text -> a
    intoText :: a -> Text

instance Textual Text where
    fromText = id
    intoText = id

instance Textual T.Text where
    fromText (UTF8 b') = T.decodeUtf8 b'
    intoText t = UTF8 (T.encodeUtf8 t)

instance Textual S.ByteString where
    fromText (UTF8 b') = b'
    intoText b' = UTF8 (T.encodeUtf8 (T.decodeUtf8 b'))

instance Textual [Char] where
    fromText (UTF8 b') = T.unpack (T.decodeUtf8 b')
    intoText cs = UTF8 (T.encodeUtf8 (T.pack cs))

data Bytes
    = StrictBytes S.ByteString
    | LazyBytes L.ByteString
    | ListBytes [Word8]
    deriving (Show, Eq)

--
-- Conversion to and from various types containing binary data into our
-- convenience Bytes type.
--
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
