{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Text.Utilities (
    Render(..),
    indefinite,

    wrap,
    underline
) where

import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as T

import Core.Text.Bytes

class Render a where
    render :: a -> Text

instance Render Text where
    render x = x

instance Render [Text] where
    render ts = UTF8 (C.concat (fmap fromText ts))

instance Render [Char] where
    render cs = intoText cs

--
-- | Render "a" or "an" in front of a word depending on English's idea of
-- whether it's a vowel or not.
--
indefinite :: Text -> Text
indefinite t =
  let
    text = fromText t
    article = if T.head text `elem` ['A','E','I','O','U','a','e','i','o','u']
        then "an "
        else "a "
    result = if T.null text
        then T.empty
        else T.append article text
  in
    intoText result

--
-- | Often the input text represents a paragraph, but does not have any
-- internal newlines (representing word wrapping). This function takes a line
-- of text and inserts newlines to simulate such folding. It also appends a
-- trailing newline to finish the paragraph.
--
wrap :: Int -> Text -> Text
wrap margin text =
  let
    built = wrapHelper margin (T.words (fromText text))
  in
    intoText (L.toStrict (T.toLazyText built))

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


underline :: Char -> Text -> Text
underline level title =
  let
    text = fromText title
    line = T.map (\_ -> level) text
  in
    intoText line

