{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
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
    ( Bytes
    , Binary(fromBytes, intoBytes)
    , hOutput
    , hInput
      {-* Internals -}
    , unBytes
    ) where

import qualified Data.ByteString as B (ByteString, foldl', splitAt
    , pack, unpack, length, hPut, hGetContents)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Builder as B (Builder, toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc
    ( Doc, emptyDoc, pretty, annotate, (<+>), hsep, vcat
    , space, punctuate, hcat, group, flatAlt, sep, fillSep
    , line, line', softline, softline', hardline
    )
import Data.Text.Prettyprint.Doc.Render.Terminal (
    color, colorDull, bold, Color(..))
import System.IO (Handle)

{-|
A block of data in binary form.
-}
data Bytes
    = StrictBytes B.ByteString
    deriving (Show, Eq, Ord, Generic)

{-|
Access the strict 'ByteString' underlying the @Bytes@ type.
-}
unBytes :: Bytes -> B.ByteString
unBytes (StrictBytes b') = b'
{-# INLINE unBytes #-}

instance Hashable Bytes

{-|
Conversion to and from various types containing binary data into our
convenience Bytes type.

As often as not these conversions are /expensive/; these methods are
here just to wrap calling the relevant functions in a uniform interface.
-}
class Binary α where
    fromBytes :: Bytes -> α
    intoBytes :: α -> Bytes

instance Binary Bytes where
    fromBytes = id
    intoBytes = id

{-| from "Data.ByteString" Strict -}
instance Binary B.ByteString where
    fromBytes (StrictBytes b') = b'
    intoBytes b' = StrictBytes b'

{-| from "Data.ByteString.Lazy" -}
instance Binary L.ByteString where
    fromBytes (StrictBytes b') = L.fromStrict b'
    intoBytes b' = StrictBytes (L.toStrict b')      -- expensive

instance Binary B.Builder where
    fromBytes (StrictBytes b') = B.byteString b'
    intoBytes b' = StrictBytes (L.toStrict (B.toLazyByteString b'))

{-| from "Data.Word" -}
instance Binary [Word8] where
    fromBytes (StrictBytes b') = B.unpack b'
    intoBytes = StrictBytes . B.pack

{-|
Output the content of the 'Bytes' to the specified 'Handle'.

@
    hOutput h b
@

'Core.Program.Execute.output' provides a convenient way to write a @Bytes@
to a file or socket handle from within the 'Core.Program.Execute.Program'
monad.

Don't use this function to write to @stdout@ if you are using any of the
other output or logging facililities of this libarary as you will corrupt
the ordering of output on the user's terminal. Instead do:

@
    'Core.Program.Execute.write' ('intoRope' b)
@

on the assumption that the bytes in question are UTF-8 (or plain ASCII)
encoded.
-}
hOutput :: Handle -> Bytes -> IO ()
hOutput handle (StrictBytes b') = B.hPut handle b'

{-|
Read the (entire) contents of a handle into a Bytes object.

If you want to read the entire contents of a file, you can do:

@
    contents <- 'Core.System.Base.withFile' name 'Core.System.Base.ReadMode' 'hInput'
@

At any kind of scale, Streaming I/O is almost always for better, but for
small files you need to pick apart this is fine.
-}
hInput :: Handle -> IO Bytes
hInput handle = do
   contents <- B.hGetContents handle
   return (StrictBytes contents)

{-
instance Show Bytes where
    show x = case x of
        StrictBytes b' -> 
-}
