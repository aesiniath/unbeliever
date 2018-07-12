{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Text
    ( Text(..)
    , Bytes(..)
    , Unicode(..)
    ) where

import qualified Data.ByteString as S (ByteString, unpack)
import qualified Data.ByteString.Lazy as L (ByteString, unpack)
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
    | StrictText T.Text
    deriving (Eq, Read, Show, Generic)

instance Hashable Text

instance IsString Text where
    fromString = StrictText . T.pack

{-
    | Stream m Chunk???
    | Rope?
-}
--  fromString :: IsString a => String -> a
--  fromUnicode :: Text -> a
class Unicode a where
    fromText :: Text -> a
    intoText :: a -> Text

instance Unicode Text where
    fromText = id
    intoText = id

instance Unicode T.Text where
    fromText x =
        case x of
            (UTF8 b') -> T.decodeUtf8 b'
            (StrictText t) -> t
    intoText t = StrictText t

instance Unicode S.ByteString where
    fromText x =
        case x of
            (UTF8 b') -> b'
            (StrictText t) -> T.encodeUtf8 t
    intoText b' = UTF8 b'

instance Unicode [Char] where
    fromText x =
        case x of
            (UTF8 b') -> T.unpack (T.decodeUtf8 b')
            (StrictText t) -> T.unpack t
    intoText cs = StrictText (T.pack cs)

data Bytes
    = StrictBytes S.ByteString
    | LazyBytes L.ByteString
    | ListBytes [Word8]
    deriving (Show, Eq)
{-
instance Show Bytes where
    show x = case x of
        StrictBytes b' -> 
-}
