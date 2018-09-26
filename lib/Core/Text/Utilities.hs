{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Text.Utilities (
      Render(..)
    , render
    , indefinite
    , wrap
    , underline
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.FingerTree as F (FingerTree(..), empty
    , singleton, (><), fromList, (<|), ViewL(..), viewl)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Short as S (ShortText, length, uncons)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty
    , defaultLayoutOptions, reAnnotateS)
import Data.Text.Prettyprint.Doc.Render.Terminal (renderStrict, AnsiStyle)

import Core.Text.Bytes
import Core.Text.Rope

-- change AnsiStyle to a custom token type, perhaps Ansi, which
-- has the escape codes already converted to Rope.

class Render a where
    type Token a :: *
    colourize :: Token a -> AnsiStyle
    intoAnsi :: a -> Doc (Token a)

{-
instance Render Rope where
    type Token Rope = 
    colourize = 
    intoAnsi x = x

instance Render [Rope] where
    intoAnsi = intoRope . F.fromList . concatMap toList . fmap unRope

instance Render [Char] where
    intoAnsi cs = intoRope cs
-}

{-|
Given an object of a type with a 'Render' instance, transform it into a
Rope saturated with ANSI escape codes representing syntax highlighting or
similar colouring.

The obvious expectation is that the next thing you're going to do is send
the Rope to console with @'Core.Program.Execute.write' (render thing)@.
However, the /better/ thing to do is to use 'Core.Program.Execute.writeR'
instead, which is able to pretty print the document text respecting the
available width of the terminal.
-}
-- the annotation (_ :: a) of the parameter is to bring type a into scope
-- at term level so that it can be used by TypedApplications. Which then
-- needed AllowAmbiguousTypes, but with all that finally it works:
-- colourize no longer needs a in its type signature.
render :: Render a => a -> Rope
render (thing :: a) = intoRope . renderStrict . reAnnotateS (colourize @a)
                . layoutPretty defaultLayoutOptions . intoAnsi $ thing

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
    snd $ foldl' (wrapLine margin) (T.length x, T.fromText x) xs

wrapLine :: Int -> (Int, T.Builder) -> T.Text -> (Int, T.Builder)
wrapLine margin (pos,builder) word =
  let
    width = T.length word
    width' = pos + width + 1
  in
    if width' > margin
        then (width , builder <> "\n" <> T.fromText word)
        else (width', builder <> " "  <> T.fromText word)


underline :: Char -> Rope -> Rope
underline level text =
  let
    title = fromRope text
    line = T.map (\_ -> level) title
  in
    intoRope line

