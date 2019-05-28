{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Useful tools for working with 'Rope's. Support for pretty printing,
multi-line strings, and...
-}
module Core.Text.Utilities (
      {-* Pretty printing -}
      Render(..)
    , render
      {-* Helpers -}
    , indefinite
    , breakWords
    , breakLines
    , breakPieces
    , wrap
    , underline
    , leftPadWith
    , rightPadWith
      {-* Multi-line strings -}
    , quote

    -- for testing
    , intoPieces
    , intoChunks

    , byteChunk
) where

import Data.Bits (Bits (..))
import Data.Char (intToDigit)
import qualified Data.ByteString as B (ByteString, splitAt, length, unpack)
import qualified Data.FingerTree as F ((<|), ViewL(..), viewl)
import qualified Data.List as List (foldl', dropWhileEnd, splitAt)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Short as S (ShortText, uncons, toText, replicate
    , singleton)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty , annotate, reAnnotateS
    , Pretty(..), pretty, emptyDoc
    , LayoutOptions(LayoutOptions)
    , PageWidth(AvailablePerLine)
    , hsep, vcat, group, flatAlt
    , softline'
    )

import Data.Text.Prettyprint.Doc.Render.Terminal (renderLazy, AnsiStyle
    , color, Color(..))

import Data.Word (Word8)
import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter))

import Core.Text.Bytes
import Core.Text.Breaking
import Core.Text.Rope

-- change AnsiStyle to a custom token type, perhaps Ansi, which
-- has the escape codes already converted to Rope.

{-|
Types which can be rendered "prettily", that is, formatted by a pretty
printer and embossed with beautiful ANSI colours when printed to the
terminal.

Use 'render' to build text object for later use or "Core.Program.Execute"'s
'Core.Program.Execute.writeR' if you're writing directly to console now.
-}

class Render α where
    {-|
Which type are the annotations of your Doc going to be expressed in?
    -}
    type Token α :: *
    {-|
Convert semantic tokens to specific ANSI escape tokens
    -}
    colourize :: Token α -> AnsiStyle
    {-|
Arrange your type as a 'Doc' @ann@, annotated with your semantic
tokens.
    -}
    intoDocA :: α -> Doc (Token α)

instance Render Rope where
    type Token Rope = ()
    colourize = const mempty
    intoDocA = foldr f emptyDoc . unRope
      where
        f :: S.ShortText -> Doc () -> Doc ()
        f piece built = (<>) (pretty (S.toText piece)) built

instance Render Char where
    type Token Char = ()
    colourize = const mempty
    intoDocA c = pretty c

instance (Render a) => Render [a] where
    type Token [a] = Token a
    colourize = const mempty
    intoDocA = mconcat . fmap intoDocA

instance Render T.Text where
    type Token T.Text = ()
    colourize = const mempty
    intoDocA t = pretty t


-- (), aka Unit, aka **1**, aka something with only one inhabitant

instance Render Bytes where
    type Token Bytes = ()
    colourize = const (color Green)
    intoDocA = prettyBytes

prettyBytes :: Bytes -> Doc ()
prettyBytes = annotate () . vcat . twoWords
    . fmap wordToHex . byteChunk . unBytes

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

byteChunk :: B.ByteString -> [B.ByteString]
byteChunk = reverse . go []
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

{-|
Given an object of a type with a 'Render' instance, transform it into a
Rope saturated with ANSI escape codes representing syntax highlighting or
similar colouring, wrapping at the specified @width@.

The obvious expectation is that the next thing you're going to do is send
the Rope to console with:

@
    'Core.Program.Execute.write' ('render' 80 thing)
@

However, the /better/ thing to do is to instead use:

@
    'Core.Program.Execute.writeR' thing
@

which is able to pretty print the document text respecting the available
width of the terminal.
-}
-- the annotation (_ :: α) of the parameter is to bring type a into scope
-- at term level so that it can be used by TypedApplications. Which then
-- needed AllowAmbiguousTypes, but with all that finally it works:
-- colourize no longer needs a in its type signature.
render :: Render α => Int -> α -> Rope
render columns (thing :: α) =
  let
    options = LayoutOptions (AvailablePerLine (columns - 1) 1.0)
  in
    intoRope . renderLazy . reAnnotateS (colourize @α)
                . layoutPretty options . intoDocA $ thing

--
-- | Render "a" or "an" in front of a word depending on English's idea of
-- whether it's a vowel or not.
--
indefinite :: Rope -> Rope
indefinite text =
  let
    x = unRope text
  in
    case F.viewl x of
        F.EmptyL -> text
        piece F.:< _ -> case S.uncons piece of
            Nothing -> text
            Just (c,_)  -> if c `elem` ['A','E','I','O','U','a','e','i','o','u']
                then intoRope ("an " F.<| x)
                else intoRope ("a " F.<| x)

{-|
Often the input text represents a paragraph, but does not have any internal
newlines (representing word wrapping). This function takes a line of text
and inserts newlines to simulate such folding, keeping the line under
the supplied maximum width.

A single word that is excessively long will be included as-is on its own
line (that line will exceed the desired maxium width).

Any trailing newlines will be removed.
-}
wrap :: Int -> Rope -> Rope
wrap margin text =
  let
    built = wrapHelper margin (breakWords text)
  in
    built

wrapHelper :: Int -> [Rope] -> Rope
wrapHelper _ [] = ""
wrapHelper _ [x]  = x
wrapHelper margin (x:xs) =
    snd $ List.foldl' (wrapLine margin) (widthRope x, x) xs

wrapLine :: Int -> (Int, Rope) -> Rope -> (Int, Rope)
wrapLine margin (pos,builder) word =
  let
    wide = widthRope word
    wide' = pos + wide + 1
  in
    if wide' > margin
        then (wide , builder <> "\n" <> word)
        else (wide', builder <> " "  <> word)


underline :: Char -> Rope -> Rope
underline level text =
  let
    title = fromRope text
    line = T.map (\_ -> level) title
  in
    intoRope line

{-|
Pad a pieve of text on the left with a specified character to the desired
width. This function is named in homage to the famous result from Computer
Science known as @leftPad@ which has a glorious place in the history of the
world-wide web.
-}
leftPadWith :: Char -> Int -> Rope -> Rope
leftPadWith c digits text =
    intoRope pad <> text
  where
    pad = S.replicate len (S.singleton c)
    len = digits - widthRope text


{-|
Right pad a text with the specified character.
-}
rightPadWith :: Char -> Int -> Rope -> Rope
rightPadWith c digits text =
    text <> intoRope pad
  where
    pad = S.replicate len (S.singleton c)
    len = digits - widthRope text


{-|
Multi-line string literals.

To use these you need to enable the @QuasiQuotes@ language extension
in your source file:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
\{\-\# LANGUAGE QuasiQuotes \#\-\}
@

you are then able to easily write a string stretching over several lines.

How best to formatting multi-line string literal within your source code is
an aesthetic judgement. Sometimes you don't care about the whitespace
leading a passage (8 spaces in this example):

@
    let message = ['quote'|
        This is a test of the Emergency Broadcast System. Do not be
        alarmed. If this were a real emergency, someone would have tweeted
        about it by now.
    |]
@

because you are feeding it into a 'Data.Text.Prettyprint.Doc.Doc' for
pretty printing and know the renderer will convert the whole text into a
single line and then re-flow it. Other times you will want to have the
string as is, literally:

@
    let poem = ['quote'|
If the sun
    rises
        in the
    west
you     drank
    too much
                last week.
    |]
@

Leading whitespace from the first line and trailing whitespace from the
last line will be trimmed, so this:

@
    let value = ['quote'|
Hello
    |]
@

is translated to:

@
    let value = 'Data.String.fromString' \"Hello\\n\"
@

without the leading newline or trailing four spaces. Note that as string
literals they are presented to your code with 'Data.String.fromString' @::
String -> α@ so any type with an 'Data.String.IsString' instance (as 'Rope'
has) can be constructed from a multi-line @['quote'| ... |]@ literal.

-}
-- I thought this was going to be more complicated.
quote :: QuasiQuoter
quote = QuasiQuoter
    (litE . stringL . trim)        -- in an expression
    (error "Cannot use [quote| ... |] in a pattern")
    (error "Cannot use [quote| ... |] as a type")
    (error "Cannot use [quote| ... |] for a declaration")
  where
    trim :: String -> String
    trim = bot . top

    top [] = []
    top ('\n':cs) = cs
    top str = str

    bot = List.dropWhileEnd (== ' ')

