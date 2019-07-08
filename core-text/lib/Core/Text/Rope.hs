{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
If you're accustomed to working with text in almost any other programming
language, you'd be aware that a \"string\" typically refers to an in-memory
/array/ of characters. Traditionally this was a single ASCII byte per
character; more recently UTF-8 variable byte encodings which dramatically
complicates finding offsets but which gives efficient support for the
entire Unicode character space. In Haskell, the original text type,
'String', is implemented as a list of 'Char' which, because a Haskell list
is implemented as a /linked-list of boxed values/, is wildly inefficient at
any kind of scale.

In modern Haskell there are two primary ways to represent text.

First is via the [rather poorly named] @ByteString@ from the __bytestring__
package (which is an array of bytes in pinned memory). The
"Data.ByteString.Char8" submodule gives you ways to manipulate those arrays
as if they were ASCII characters. Confusingly there are both strict
(@Data.ByteString@) and lazy (@Data.ByteString.Lazy@) variants which are
often hard to tell the difference between when reading function signatures
or haddock documentation. The performance problem an immutable array backed
data type runs into is that appending a character (that is, ASCII byte) or
concatonating a string (that is, another array of ASCII bytes) is very
expensive and requires allocating a new larger array and copying the whole
thing into it. This led to the development of \"builders\" which amortize
this reallocation cost over time, but it can be cumbersome to switch
between @Builder@, the lazy @ByteString@ that results, and then having to
inevitably convert to a strict @ByteString@ because that's what the next
function in your sequence requires.

The second way is through the opaque @Text@ type of "Data.Text" from the
__text__ package, which is well tuned and high-performing but suffers from
the same design; it is likewise backed by arrays. Rather surprisingly, the
storage backing Text objects are encoded in UTF-16, meaning every time you
want to work with unicode characters that came in from /anywhere/ else and
which inevitably are UTF-8 encoded you have to convert to UTF-16 and copy
into a new array, wasting time and memory.

In this package we introduce 'Rope', a text type backed by the 2-3
'Data.FingerTree.FingerTree' data structure from the __fingertree__
package. This is not an uncommon solution in many languages as finger trees
support exceptionally efficient appending to either end and good
performance inserting anywhere else (you often find them as the backing
data type underneath text editors for this reason). Rather than 'Char' the
pieces of the rope are 'Data.Text.Short.ShortText' from the __text-short__
package, which are UTF-8 encoded and in normal memory managed by the
Haskell runtime. Conversion from other Haskell text types is not /O(1)/
(UTF-8 validity must be checked, or UTF-16 decoded, or...), but in our
benchmarking the performance has been comparable to the established types
and you may find the resultant interface for combining chunks is comparable
to using a Builder, without being forced to use a Builder.

'Rope' is used as the text type throughout this library. If you use the
functions within this package (rather than converting to other text types)
operations are quite efficient. When you do need to convert to another type
you can use 'fromRope' or 'intoRope' from the 'Textual' typeclass.

Note that we haven't tried to cover the entire gamut of operations or
customary convenience functions you would find in the other libraries; so
far 'Rope' is concentrated on aiding interoperation, being good at
appending (lots of) small pieces, and then efficiently taking the resultant
text object out to a file handle, be that the terminal console, a file, or
a network socket.

-}
module Core.Text.Rope
    ( {-* Rope type -}
      Rope
    , emptyRope
    , widthRope
    , splitRope
    , insertRope
    , containsCharacter
      {-* Interoperation and Output -}
    , Textual(fromRope, intoRope, appendRope)
    , hWrite
      {-* Internals -}
    , unRope
    , nullRope
    , unsafeIntoRope
    , Width(..)
    ) where

import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Builder as B (toLazyByteString
    , hPutBuilder)
import qualified Data.ByteString.Lazy as L (ByteString, toStrict
    , foldrChunks)
import qualified Data.FingerTree as F (FingerTree, Measured(..), empty
    , singleton, (><), (<|), (|>), search, SearchResult(..), null
    , viewl, ViewL(..))
import Data.Foldable (foldr, foldr', foldMap, toList, any)
import Data.Hashable (Hashable, hashWithSalt)
import Data.String (IsString(..))
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text, fromChunks, foldrChunks
    , toStrict)
import qualified Data.Text.Lazy.Builder as U (Builder, toLazyText
    , fromText)
import Data.Text.Prettyprint.Doc (Pretty(..), emptyDoc)
import qualified Data.Text.Short as S (ShortText, length, any, null
    , fromText, toText, fromByteString, pack, unpack
    , append, empty, toBuilder, splitAt)
import qualified Data.Text.Short.Unsafe as S (fromByteStringUnsafe)
import GHC.Generics (Generic)
import System.IO (Handle)

import Core.Text.Bytes

{-|
A type for textual data. A rope is text backed by a tree data structure,
rather than a single large continguous array, as is the case for strings.

There are three use cases:

/Referencing externally sourced data/

Often we interpret large blocks of data sourced from external systems as
text. Ideally we would hold onto this without copying the memory, but (as
in the case of @ByteString@ which is the most common source of data) before
we can treat it as text we have to validate the UTF-8 content. Safety
first. We also copy it out of pinned memory, allowing the Haskell runtime
to manage the storage.

/Interoperating with other libraries/

The only constant of the Haskell universe is that you won't have the right
combination of {strict, lazy} × {@Text@, @ByteString@, @String@, @[Word8]@,
etc} you need for the next function call. The 'Textual' typeclass provides
for moving between different text representations. To convert between
@Rope@ and something else use 'fromRope'; to construct a @Rope@ from
textual content in another type use 'intoRope'.

You can get at the underlying finger tree with the 'unRope' function.

/Assembling text to go out/

This involves considerable appending of data, very very occaisionally
inserting it. Often the pieces are tiny. To add text to a @Rope@ use the
'appendRope' method as below or the ('Data.Semigroup.<>') operator from
"Data.Monoid" (like you would have with a @Builder@).

Output to a @Handle@ can be done efficiently with 'hWrite'.
-}
data Rope
    = Rope (F.FingerTree Width S.ShortText)
    deriving Generic

instance NFData Rope where
    rnf (Rope x) = foldMap (\piece -> rnf piece) x

instance Show Rope where
    show text = "\"" ++ fromRope text ++ "\""

instance Eq Rope where
    (==) (Rope x1) (Rope x2) = (==) (stream x1) (stream x2)
      where
        stream x = foldMap S.unpack x

instance Ord Rope where
    compare (Rope x1) (Rope x2) = compare x1 x2

instance Pretty Rope where
    pretty (Rope x) = foldr ((<>) . pretty . S.toText) emptyDoc x 

{-|
Access the finger tree underlying the @Rope@. You'll want the following
imports:

@
import qualified "Data.FingerTree" as F  -- from the __fingertree__ package
import qualified "Data.Text.Short" as S  -- from the __text-short__ package
@
-}
unRope :: Rope -> F.FingerTree Width S.ShortText
unRope (Rope x) = x
{-# INLINE unRope #-}


{-|
The length of the @Rope@, in characters. This is the monoid used to
structure the finger tree underlying the @Rope@.
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
    fromString "" = emptyRope
    fromString xs = Rope . F.singleton . S.pack $ xs

instance Semigroup Rope where
    (<>) text1@(Rope x1) text2@(Rope x2) =
        if F.null x2
            then text1
            else if F.null x1
                then text2
                else Rope ((F.><) x1 x2) -- god I hate these operators

instance Monoid Rope where
    mempty = emptyRope
    mappend = (<>)

{-|
An zero-length 'Rope'. You can also use @\"\"@ presuming the
__@OverloadedStrings@__ language extension is turned on in your source
file.
-}
emptyRope :: Rope
emptyRope = Rope F.empty
{-# INLINABLE emptyRope #-}

{-|
Get the length of this text, in characters.
-}
widthRope :: Rope -> Int
widthRope = foldr' f 0 . unRope
  where
    f piece count = S.length piece + count

nullRope :: Rope -> Bool
nullRope (Rope x) = case F.viewl x of
    F.EmptyL        -> True
    (F.:<) piece _  -> S.null piece

{-|
Break the text into two pieces at the specified offset.

Examples:

@
λ> __splitRope 0 \"abcdef\"__
(\"\", \"abcdef\")
λ> __splitRope 3 \"abcdef\"__
(\"abc\", \"def\")
λ> __splitRope 6 \"abcdef\"__
(\"abcdef\",\"\")
@

Going off either end behaves sensibly:

@
λ> __splitRope 7 \"abcdef\"__
(\"abcdef\",\"\")
λ> __splitRope (-1) \"abcdef\"__
(\"\", \"abcdef\")
@
-}
splitRope :: Int -> Rope -> (Rope,Rope)
splitRope i text@(Rope x) =
  let
    pos = Width i
    result = F.search (\w1 _ -> w1 >= pos) x
  in
    case result of
        F.Position before piece after ->
          let
            (Width w) = F.measure before
            (one,two) = S.splitAt (i - w) piece
          in
            (Rope ((F.|>) before one),Rope ((F.<|) two after))
        F.OnLeft -> (Rope F.empty, text)
        F.OnRight -> (text, Rope F.empty)
        F.Nowhere -> error "Position not found in split. Probable cause: predicate function given not monotonic. This is supposed to be unreachable"

{-|
Insert a new piece of text into an existing @Rope@ at the specified offset.

Examples:

@
λ> __insertRope 3 \"Con\" \"Def 1\"__
"DefCon 1"
λ> __insertRope 0 \"United \" \"Nations\"__
"United Nations"
@
-}
insertRope :: Int -> Rope -> Rope -> Rope
insertRope 0 (Rope new) (Rope x) = Rope ((F.><) new x)
insertRope i (Rope new) text =
  let
    (Rope before,Rope after) = splitRope i text
  in
    Rope (mconcat [before, new, after])

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
        f piece num = hashWithSalt num piece

{-|
Machinery to interpret a type as containing valid Unicode that can be
represented as a @Rope@ object.

/Implementation notes/

Given that @Rope@ is backed by a finger tree, 'append' is relatively
inexpensive, plus whatever the cost of conversion is. There is a subtle
trap, however: if adding small fragments of that were obtained by slicing
(for example) a large ByteString we would end up holding on to a reference
to the entire underlying block of memory. This module is optimized to
reduce heap fragmentation by letting the Haskell runtime and garbage
collector manage the memory, so instances are expected to /copy/ these
substrings out of pinned memory.

The @ByteString@ instance requires that its content be valid UTF-8. If not
an empty @Rope@ will be returned.

Several of the 'fromRope' implementations are expensive and involve a lot
of intermediate allocation and copying. If you're ultimately writing to a
handle prefer 'hWrite' which will write directly to the output buffer.
-}
class Textual α where
    {-|
Convert a @Rope@ into another text-like type.
    -}
    fromRope :: Rope -> α
    {-|
Take another text-like type and convert it to a @Rope@.
    -}
    intoRope :: α -> Rope
    {-|
Append some text to this @Rope@. The default implementation is basically a
convenience wrapper around calling 'intoRope' and 'mappend'ing it to your
text (which will work just fine, but for some types more efficient
implementations are possible).
    -}
    appendRope :: α -> Rope -> Rope
    appendRope thing text = text <> intoRope thing

instance Textual (F.FingerTree Width S.ShortText) where
    fromRope = unRope
    intoRope = Rope

instance Textual Rope where
    fromRope = id
    intoRope = id
    appendRope (Rope x2) (Rope x1) = Rope ((F.><) x1 x2)

{-| from "Data.Text.Short" -}
instance Textual S.ShortText where
    fromRope = foldr S.append S.empty . unRope
    intoRope = Rope . F.singleton
    appendRope piece (Rope x) = Rope ((F.|>) x piece)

{-| from "Data.Text" Strict -}
instance Textual T.Text where
    fromRope = U.toStrict . U.toLazyText . foldr f mempty . unRope
      where
        f :: S.ShortText -> U.Builder -> U.Builder
        f piece built = (<>) (U.fromText (S.toText piece)) built

    intoRope t = Rope (F.singleton (S.fromText t))
    appendRope chunk (Rope x) = Rope ((F.|>) x (S.fromText chunk))

{-| from "Data.Text.Lazy" -}
instance Textual U.Text where
    fromRope (Rope x) = U.fromChunks . fmap S.toText . toList $ x
    intoRope t = Rope (U.foldrChunks ((F.<|) . S.fromText) F.empty t)

{-| from "Data.ByteString" Strict -}
instance Textual B.ByteString where
    fromRope = L.toStrict . B.toLazyByteString . foldr g mempty . unRope
      where
        g piece built = (<>) (S.toBuilder piece) built

    -- If the input ByteString does not contain valid UTF-8 then an empty
    -- Rope will be returned. That's not ideal.
    intoRope b' = case S.fromByteString b' of
        Just piece -> Rope (F.singleton piece)
        Nothing -> Rope F.empty         -- bad

    -- ditto
    appendRope b' (Rope x) = case S.fromByteString b' of
        Just piece -> Rope ((F.|>) x piece)
        Nothing -> (Rope x)             -- bad

{-| from "Data.ByteString.Lazy" -}
instance Textual L.ByteString where
    fromRope = B.toLazyByteString . foldr g mempty . unRope
      where
        g piece built = (<>) (S.toBuilder piece) built

    intoRope b' = Rope (L.foldrChunks ((F.<|) . check) F.empty b')
      where
        check chunk = case S.fromByteString chunk of
            Just piece -> piece
            Nothing -> S.empty          -- very bad

instance Textual Bytes where
    fromRope = intoBytes . (fromRope :: Rope -> B.ByteString)
    intoRope = intoRope . unBytes

instance Binary Rope where
    fromBytes = intoRope . unBytes
    intoBytes = intoBytes . (fromRope :: Rope -> B.ByteString)


{-|
If you /know/ the input bytes are valid UTF-8 encoded characters, then
you can use this function to convert to a piece of @Rope@.
-}
unsafeIntoRope :: B.ByteString -> Rope
unsafeIntoRope = Rope . F.singleton . S.fromByteStringUnsafe

{-| from "Data.String" -}
instance Textual [Char] where
    fromRope (Rope x) = foldr h [] x
      where
        h piece string = (S.unpack piece) ++ string -- ugh
    intoRope = Rope . F.singleton . S.pack

{-|
Write the 'Rope' to the given 'Handle'.

@
import "Core.Text"
import "Core.System" -- re-exports stdout

main :: IO ()
main =
  let
    text :: 'Rope'
    text = "Hello World"
  in
    'hWrite' 'System.IO.stdout' text
@
because it's tradition.

Uses 'Data.ByteString.Builder.hPutBuilder' internally which saves all kinds
of intermediate allocation and copying because we can go from the
'Data.Text.Short.ShortText's in the finger tree to
'Data.ByteString.Short.ShortByteString' to
'Data.ByteString.Builder.Builder' to the 'System.IO.Handle''s output buffer
in one go.

If you're working in the 'Core.Program.Execute.Program' monad, then
'Core.Program.Execute.write' provides an efficient way to write a @Rope@ to
@stdout@.
-}
hWrite :: Handle -> Rope -> IO ()
hWrite handle (Rope x) = B.hPutBuilder handle (foldr j mempty x)
  where
    j piece built = (<>) (S.toBuilder piece) built

{-|
Does the text contain this character?

We've used it to ask whether there are newlines present in a @Rope@, for
example:

@
    if 'containsCharacter' \'\n\' text
        then handleComplexCase
        else keepItSimple
@
-}
containsCharacter :: Char -> Rope -> Bool
containsCharacter q (Rope x) = any j x
  where
    j piece = S.any (\c -> c == q) piece
