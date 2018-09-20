{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Text.Utilities (
      Render(..)
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

import Core.Text.Bytes
import Core.Text.Rope

class Render a where
    render :: a -> Rope

instance Render Rope where
    render x = x

instance Render [Rope] where
    render = Rope . F.fromList . concatMap toList . fmap unRope

instance Render [Char] where
    render cs = intoRope cs

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
                then Rope ("an " F.<| x)
                else Rope ("a " F.<| x)


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

