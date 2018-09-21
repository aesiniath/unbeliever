{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Core.Text.Rope
    ( Rope
    , Width(..)
    , unRope
    , width
    , contains
    , Textual(..)
    , hOutput
    ) where

import qualified Data.ByteString as B (ByteString, unpack, empty, append)
import qualified Data.ByteString.Builder as B (Builder, toLazyByteString
    , hPutBuilder)
import qualified Data.ByteString.Lazy as L (toStrict)
import Data.String (IsString(..))
import qualified Data.FingerTree as F (FingerTree, Measured(..), empty
    , singleton, (><), (<|), (|>))
import Data.Foldable (foldr, foldr', foldMap, toList, any)
import qualified Data.Text as T (Text, empty, append)
import qualified Data.Text.Lazy as U (Text, fromChunks, foldrChunks
    , toStrict)
import qualified Data.Text.Lazy.Builder as U (Builder, toLazyText
    , fromText)
import qualified Data.Text.Short as S (ShortText, length, any
    , fromText, toText, fromByteString, toByteString, pack, unpack
    , concat, append, empty, toBuilder)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import GHC.Generics (Generic)
import System.IO (Handle)

{-|

There are two use cases: first, referencing large blocks of data sourced from
external systems. Ideally we would hold onto this without copying the memory.
ByteString and its pinned memory is appropriate for this.

 ... maybe that's what Bytes is for

However, if we are manipulating this /at all/ in any way we're going to end 
up needing to copy it ... is that true?

Second use case is assembling text to go out. This involves considerable
appending of data, very very occaisionally inserting it. Often the pieces
are tiny.

-}

data Rope
    = Rope (F.FingerTree Width S.ShortText)
    deriving Generic

instance Show Rope where
    show text = "\"" ++ fromRope text ++ "\""

instance Eq Rope where
    (==) (Rope x1) (Rope x2) = (==) (stream x1) (stream x2)
      where
        stream x = foldMap S.unpack x


{-|
Access the finger tree underlying the 'Rope'. You'll want the following
imports:

@
import qualified Data.FingerTree as F  -- from the __fingertree__ package
import qualified Data.Text.Short as S  -- from the __text-short__ package
@
-}
unRope :: Rope -> F.FingerTree Width S.ShortText
unRope (Rope x) = x
{-# INLINE unRope #-}


{-|
The length of the Rope, in characters. This is the monoid used to structure
the finger tree underlying the Rope.
-}
newtype Width = Width Int
    deriving (Eq, Ord, Show, Num, Generic)

instance F.Measured Width S.ShortText where
    measure :: S.ShortText -> Width
    measure piece = Width (S.length piece)

instance Semigroup Width where
    (<>) (Width w1) (Width w2) = Width (w1 + w2)

instance Monoid Width where
    mempty = Width 0
    mappend = (<>)

-- here Maybe we just need type Strand = ShortText and then Rope is
-- FingerTree Strand or Builder (Strand)

instance IsString Rope where
    fromString = Rope . F.singleton . S.pack

instance Semigroup Rope where
    (<>) (Rope x1) (Rope x2) = Rope ((F.><) x1 x2) -- god I hate these operators

instance Monoid Rope where
    mempty = Rope F.empty
    mappend = (<>)

width :: Rope -> Int
width = foldr' f 0 . unRope
  where
    f piece count = S.length piece + count

--
-- Manual instance to get around the fact that FingerTree doesn't have a
-- Hashable instance. If this were ever to become a hotspot we could
-- potentially use the Hashed caching type in the finger tree as
--
-- FingerTree Width (Hashed S.ShortText)
--
-- at the cost of endless unwrapping.
--
instance Hashable Rope where
    hashWithSalt salt (Rope x) = foldr f salt x
      where
        f :: S.ShortText -> Int -> Int
        f piece salt = hashWithSalt salt piece

{-|
Machinery to interpret a type as containing valid UTF-8 that can be
represented as a Rope object.

/Implementation notes/

Given that Rope is backed by a finger tree, 'append' is relatively
inexpensive, plus whatever the cost of conversion is. There is a subtle
trap, however: if adding small fragments of that were obtained by slicing
(for example) a large ByteString we would end up holding on to a reference
to the entire underlying pinned memory.

This module is optimized to reduce heap fragmentation by letting the
Haskell runtime and garbage collector manage the memory

Instances are expected to /copy/ these strings out of pinned memory.

Several of the 'fromRope' implementations are expensive and involves a lot
of intermiate allocation and copying. If you're ultimately writing to a
handle prefer 'hOutput' which will write directly to the output buffer
-}
class Textual a where
    fromRope :: Rope -> a
    intoRope :: a -> Rope

{-|
Append some text to this Rope. The default implementation is basically a
convenience wrapper around calling 'intoRope' and the 'mappend'ing it to
your text, but for many types more efficient implementations are provided.
-}
    append :: a -> Rope -> Rope append thing text = text <> intoRope thing

instance Textual (F.FingerTree Width S.ShortText) where
    fromRope = unRope
    intoRope = Rope

instance Textual Rope where
    fromRope = id
    intoRope = id

instance Textual S.ShortText where
    fromRope = foldr S.append S.empty . unRope
    intoRope = Rope . F.singleton
    append piece (Rope x) = Rope ((F.|>) x piece)

instance Textual T.Text where
    fromRope = U.toStrict . U.toLazyText . foldr f mempty . unRope
      where
        f :: S.ShortText -> U.Builder -> U.Builder
        f piece built = (<>) (U.fromText (S.toText piece)) built

    intoRope t = Rope (F.singleton (S.fromText t))
    append piece (Rope t) = Rope ((F.|>) t (S.fromText piece))

instance Textual U.Text where
    fromRope (Rope x) = U.fromChunks . fmap S.toText . toList $ x
    intoRope t = Rope (U.foldrChunks ((F.<|) . S.fromText) F.empty t)

instance Textual B.ByteString where
    fromRope = L.toStrict . B.toLazyByteString . foldr g mempty . unRope
      where
        g piece built = (<>) (S.toBuilder piece) built

    -- If the input ByteString does not contain valid UTF-8 then an empty
    -- Rope will be returned. That's not ideal.
    intoRope b' = case S.fromByteString b' of
        Just piece -> Rope (F.singleton piece)
        Nothing -> Rope F.empty

instance Textual [Char] where
    fromRope (Rope x) = foldr h [] x
      where
        h piece string = (S.unpack piece) ++ string -- ugh
    intoRope = Rope . F.singleton . S.pack

{-|
Write the 'Rope' to the given 'Handle'. Uses
'Data.ByteString.Builder.hPutBuilder' internally which saves all kinds of
intermediate allocation and copying because we can go from the 'ShortText's
in the finger tree to 'ShortByteString' to 'Builder' to the 'Handle''s
output buffer in one go.
-}
hOutput :: Handle -> Rope -> IO ()
hOutput handle (Rope x) = B.hPutBuilder handle (foldr j mempty x)
  where
    j piece built = (<>) (S.toBuilder piece) built

{-|
Does this Text contain this character?

We've used it to ask whether there are newlines present, for
example:

@
    if 'contains' '\n' text
        then handleComplexCase
        else keepItSimple
@
-}
contains :: Char -> Rope -> Bool
contains q (Rope x) = any j x
  where
    j piece = S.any (\c -> c == q) piece
