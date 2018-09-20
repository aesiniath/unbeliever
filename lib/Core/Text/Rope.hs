{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Core.Text.Rope
    ( Rope(..)
    , Width(..)
    , unRope
    , contains
    , Textual(..)
    ) where

import qualified Data.ByteString as B (ByteString, unpack, empty, append)
import Data.String (IsString(..))
import qualified Data.FingerTree as F (FingerTree, Measured(..), empty
    , singleton, (><), (<|))
import Data.Foldable (foldr, toList, any)
import qualified Data.Text as T (Text, empty, append)
import qualified Data.Text.Lazy as U (Text, fromChunks, foldrChunks)
import qualified Data.Text.Short as S (ShortText, length, pack, any
    , fromText, toText, fromByteString, toByteString, fromString, toString
    , concat, append, empty)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

{-|

There are two use cases: first, referencing large blocks of data sourced from
external systems. Ideally we would hold onto this without copying the memory.
ByteString and its pinned memory is appropriate for this.

 ... maybe that's what Bytes is for

However, if we are manipulating this /at all/ in any way we're going to end 
up needing to copy it ... is that true?

Second use case is assembling text to go out. This involves considerable
appending of data, very very occaisionally inserting it. Often the pieces are
tiny.

-}

data Rope
    = Rope (F.FingerTree Width S.ShortText)
    deriving Generic

{-|
Access the finger tree underlying the 'Rope'. You'll want the following
imports:

@
import qualified Data.FingerTree as F  -- from the **fingertree** package
import qualified Data.Text.Short as S  -- from the **text-short** package
@
-}
unRope :: Rope -> F.FingerTree Width S.ShortText
unRope (Rope x) = x


{-|
The length of the Rope, in characters.
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

--  fromString :: IsString a => String -> a
--  fromTextual :: Text -> a

{-|
Machinery to interpret a type as containing valid UTF-8 that can be
represented as a Text object.
-}
class Textual a where
    fromRope :: Rope -> a
    intoRope :: a -> Rope

instance Textual Rope where
    fromRope = id
    intoRope = id

instance Textual S.ShortText where
    fromRope = foldr S.append S.empty . unRope
    intoRope = Rope . F.singleton

-- FIXME Wow. Use Text's Builder instead?
instance Textual T.Text where
    fromRope (Rope x) = foldr f T.empty x
      where
        f piece text = T.append text (S.toText piece)
    intoRope t = Rope (F.singleton (S.fromText t))

instance Textual U.Text where
    fromRope (Rope x) = U.fromChunks . fmap S.toText . toList $ x
    intoRope t = Rope (U.foldrChunks ((F.<|) . S.fromText) F.empty t)

-- FIXME Same thing again. Use ByteString's Builder instead?
instance Textual B.ByteString where
    fromRope (Rope x) = foldr g B.empty x
      where
        g piece bytes = B.append bytes (S.toByteString piece) -- UTF8 throughout

    {-| If the input ByteString does not contain valid UTF-8 then an empty Rope will be returned -}
    intoRope b' = case S.fromByteString b' of
        Just piece -> Rope (F.singleton piece)
        Nothing -> Rope F.empty

instance Textual [Char] where
    fromRope (Rope x) = foldr h [] x
      where
        h piece string = (++) string (S.toString piece) -- ugh
    intoRope = Rope . F.singleton . S.pack

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
