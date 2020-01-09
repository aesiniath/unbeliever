{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module, hidden from Haddock
module Core.Text.Parsing
    ( calculatePositionEnd
    )
where

import Data.Foldable (foldl)
import qualified Data.Text.Short as S (ShortText, foldl)

import Core.Text.Rope

{-|
Calculate the line number and column number of a Rope (interpreting it as
if is a block of text in a file). By the convention observed by all leading
brands of text editor, lines and columns are @1@ origin, so an empty Rope
is position @(1,1)@.
-}
-- Of course, if Rope itself cached position information in the FingerTree
-- monoid this would be trivial.

calculatePositionEnd :: Rope -> (Int,Int)
calculatePositionEnd text =
  let
    x = unRope text
    (l,c) = foldl calculateChunk (1,1) x
  in
    (l,c)

calculateChunk :: (Int,Int) -> S.ShortText -> (Int,Int)
calculateChunk loc piece =
    S.foldl f loc piece
  where
    f :: (Int,Int) -> Char -> (Int,Int)
    f (l,c) ch = if ch == '\n'
        then (l+1,1)
        else (l,c+1)