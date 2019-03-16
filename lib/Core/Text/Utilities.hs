{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
      {-* Multi-line strings -}
    , quote
) where

import Data.Char (isSpace)
import qualified Data.FingerTree as F ((<|), ViewL(..), viewl, singleton)
import qualified Data.List as List (foldl', dropWhileEnd)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Short as S (ShortText, uncons, toText, empty
    , null, breakEnd, append, dropEnd)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty , reAnnotateS
    , pretty, emptyDoc
    , LayoutOptions(LayoutOptions)
    , PageWidth(AvailablePerLine))
import Data.Text.Prettyprint.Doc.Render.Terminal (renderLazy, AnsiStyle)
import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter))

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
Split a passage of text into a list of words. A line is broken wherever
there is one or more whitespace characters, as defined by "Data.Char"'s
'Data.Char.isSpace'.

Examples:

@
λ> __breakWords \"This is a test\"__
[\"This\",\"is\",\"a\",\"test\"]
λ> __breakWords (\"St\" <> \"op and \" <> \"go left\")__
[\"Stop\",\"and\",\"go\",\"left\"]
λ> __breakWords emptyRope__
[]
@
-}
breakWords :: Rope -> [Rope]
breakWords = filter (not . nullRope) . breakPieces isSpace

{-|
Split a paragraph of text into a list of its individual lines. The
paragraph will be broken wherever there is a @'\n'@ character.
-}
breakLines :: Rope -> [Rope]
breakLines text = breakPieces isNewline text

isNewline :: Char -> Bool
isNewline c = c == '\n'

{-|
Break a Rope into pieces whereever the given predicate function returns
@True@. If found, that character will not be included on either side. Empty
runs, however, *will* be preserved.
-}
breakPieces :: (Char -> Bool) -> Rope -> [Rope]
breakPieces predicate text =
  let
    (final,list) = foldr (finder predicate) (S.empty,[]) (unRope text)
    l = intoRope (F.singleton final)
  in
    if S.null final
        then list
        else l:list

finder
    :: (Char -> Bool)
    -> S.ShortText
    -> (S.ShortText,[Rope])
    -> (S.ShortText,[Rope])
finder predicate piece (accum,list) =
  let
    done = S.null piece

    -- λ> S.breakEnd isSpace "a d"
    -- ("a ","d")
    --
    -- λ> S.breakEnd isSpace " and"
    -- (" ","and")
    --
    -- λ> S.breakEnd isSpace "and "
    -- ("and ","")
    --
    -- λ> S.breakEnd isSpace ""
    -- ("","")
    --
    -- λ> S.breakEnd isSpace " "
    -- (" ","")

    (remainder,fragment) = S.breakEnd predicate piece

    -- Are we in the middle of a word? We are if the carry forward is
    -- non-zero length.
    --
    -- Did we find a word in the current piece? If so, then if we are in
    -- the middle of accumulating a word, we add the new piece to it.

    found  = not (S.null fragment)
    middle = not (S.null accum)

    accum' = if found
                then if middle
                    then S.append fragment accum
                    else fragment
                else accum

    -- Did we find a space? We did if remainder is non-zero length.
    -- Finding a space means flushing out the accumulator (though only if
    -- there's actually something there). We have to drop that whitespace
    -- before iterating.

    space = not (S.null remainder)
    empty = S.null accum'
    word = intoRope accum'

    list' = if empty
                then list
                else word:list

    remainder' = S.dropEnd 1 remainder
  in
    if done
        then (accum',list)
        else if space
            then finder predicate remainder' (S.empty,list')
            else finder predicate remainder (accum',list)

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

