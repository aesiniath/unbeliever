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
      Render(..)
    , render
    , indefinite
    , wrap
    , underline
      {-* Multi-line strings -}
    , quote
) where

import qualified Data.FingerTree as F ((<|), ViewL(..), viewl)
import qualified Data.List as List (foldl', dropWhile, dropWhileEnd)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Short as S (uncons)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty , reAnnotateS
    , LayoutOptions(LayoutOptions)
    , PageWidth(AvailablePerLine))
import Data.Text.Prettyprint.Doc.Render.Terminal (renderStrict, AnsiStyle)
import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter))

import Core.Text.Rope

-- change AnsiStyle to a custom token type, perhaps Ansi, which
-- has the escape codes already converted to Rope.

{-|
Types which can be rendered "prettily", that is, formatted by a pretty
printer and embossed with beautiful ANSI colours when printed to the
terminal.

Use 'render' to build text object for later use or "Core.Program"'s
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

{-
instance Render Rope where
    type Token Rope = 
    colourize = 
    intoDocA x = x

instance Render [Rope] where
    intoDocA = intoRope . F.fromList . concatMap toList . fmap unRope

instance Render [Char] where
    intoDocA cs = intoRope cs
-}

{-|
Given an object of a type with a 'Render' instance, transform it into a
Rope saturated with ANSI escape codes representing syntax highlighting or
similar colouring, wrapping at the specified @width@.

The obvious expectation is that the next thing you're going to do is send
the Rope to console with @'Core.Program.Execute.write' (render thing)@.
However, the /better/ thing to do is to use 'Core.Program.Execute.writeR'
instead, which is able to pretty print the document text respecting the
available width of the terminal.
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
    intoRope . renderStrict . reAnnotateS (colourize @α)
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
and inserts newlines to simulate such folding. It also appends a trailing
newline to finish the paragraph.
-}
wrap :: Int -> Rope -> Rope
wrap margin text =
  let
    built = wrapHelper margin (T.words (fromRope text))
  in
    intoRope (T.toLazyText built)

wrapHelper :: Int -> [T.Text] -> T.Builder
wrapHelper _ [] = ""
wrapHelper _ [x]  = T.fromText x
wrapHelper margin (x:xs) =
    snd $ List.foldl' (wrapLine margin) (T.length x, T.fromText x) xs

wrapLine :: Int -> (Int, T.Builder) -> T.Text -> (Int, T.Builder)
wrapLine margin (pos,builder) word =
  let
    wide = T.length word
    wide' = pos + wide + 1
  in
    if wide' > margin
        then (wide , builder <> "\n" <> T.fromText word)
        else (wide', builder <> " "  <> T.fromText word)


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

