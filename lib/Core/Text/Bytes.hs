{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}        -- FIXME
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- FIXME

{-|
Binary (as opposed to textual) data is encountered in weird corners of the
Haskell ecosystem. We tend to forget (for example) that the content
recieved from a web server is /not/ text until we convert it from UTF-8 (if
that's what it is); and of course that glosses over the fact that something
of content-type @image/jpeg@ is not text in any way, shape, or form.

Bytes also show up when working with crypto algorithms, taking hashes, and
when doing serialization to external binary formats. Although we frequently
display these in terminals (and in URLs!) as text, but we take for granted
that we have actually deserialized the data or rendered the it in
hexidecimal or base64 or...

This module presents a simple wrapper around various representations of
binary data to make it easier to interoperate with libraries supplying
or consuming bytes.
-}
module Core.Text.Bytes
    ( Bytes(..)
    , Binary
    , fromBytes
    , intoBytes
    ) where

import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)

{-|
A block of data in binary form.
-}
data Bytes
    = StrictBytes S.ByteString
    | LazyBytes L.ByteString
    | ListBytes [Word8]
    deriving (Show, Eq)

{-|
Conversion to and from various types containing binary data into our
convenience Bytes type.

As often as not these conversions are /expensive/; these methods are
here just to wrap calling the relevant functions in a uniform interface.
-}
class Binary α where
    fromBytes :: Bytes -> α
    intoBytes :: α -> Bytes

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
