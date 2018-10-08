{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}        -- FIXME
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- FIXME
{-# OPTIONS_HADDOCK prune #-}

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
    , Binary(fromBytes, intoBytes)
    , chunk
    ) where

import Data.Bits (Bits (..))
import Data.Char (intToDigit)
import qualified Data.ByteString as B (ByteString, foldl', splitAt
    , unpack, length)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc
    ( Doc, emptyDoc, pretty, annotate, (<+>), hsep, vcat
    , space, punctuate, hcat, group, flatAlt, sep, fillSep
    , line, line', softline, softline', hardline
    )
import Data.Text.Prettyprint.Doc.Render.Terminal (
    color, colorDull, bold, Color(..))

import Core.Text.Utilities

{-|
A block of data in binary form.
-}
data Bytes
    = StrictBytes B.ByteString
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

instance Binary B.ByteString where
    fromBytes (StrictBytes b') = b'
    intoBytes b' = StrictBytes b'

instance Binary L.ByteString where
    fromBytes (StrictBytes b') = L.fromStrict b'
    intoBytes b' = StrictBytes (L.toStrict b')      -- expensive

-- (), aka Unit, aka **1**, aka something with only one inhabitant

instance Render Bytes where
    type Token Bytes = ()
    colourize = const (color Green)
    intoDocA = prettyBytes
    
prettyBytes :: Bytes -> Doc ()
prettyBytes (StrictBytes b') = annotate () . vcat . twoWords
    . fmap wordToHex . chunk $ b'

twoWords :: [Doc ann] -> [Doc ann]
twoWords ds = go ds
  where
    go [] = []
    go [x] = [softline' <> x]
    go xs =
      let
        (one:two:[], remainder) = List.splitAt 2 xs
      in
        group (one <> spacer <> two) : go remainder

    spacer = flatAlt softline' "  "


chunk :: B.ByteString -> [B.ByteString]
chunk = reverse . go []
  where
    go acc blob =
      let
        (eight, remainder) = B.splitAt 8 blob
      in
        if B.length remainder == 0
            then eight : acc
            else go (eight : acc) remainder

-- Take an [up to] 8 byte (64 bit) word
wordToHex :: B.ByteString -> Doc ann
wordToHex eight =
  let
    ws = B.unpack eight
    ds = fmap byteToHex ws
  in
    hsep ds

byteToHex :: Word8 -> Doc ann
byteToHex c = pretty hi <> pretty low
  where
    !low      = byteToDigit $ c .&. 0xf
    !hi       = byteToDigit $ (c .&. 0xf0) `shiftR` 4

    byteToDigit :: Word8 -> Char
    byteToDigit = intToDigit . fromIntegral

{-
instance Show Bytes where
    show x = case x of
        StrictBytes b' -> 
-}
