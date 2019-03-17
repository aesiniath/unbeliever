{-# LANGUAGE OverloadedStrings #-}

module Core.Text.Breaking where

import qualified Data.Text.Short as S
import Data.Foldable (foldl')
import Data.List (uncons)

import Core.Text.Rope

{-
Was the previous piece a match, or are we in the middle of a run of
characters? If we were, then join the previous run to the current piece
before processing into chunks.
-}
-- now for right fold
intoPieces :: (Char -> Bool) -> S.ShortText -> [S.ShortText] -> [S.ShortText]
intoPieces predicate piece list =
  let
    (list',piece') = case uncons list of
        Nothing -> ([],piece)
        Just (previous,remainder) -> if S.null previous
            then (list,piece)
            else (remainder,piece <> previous)

    pieces = intoChunks predicate piece'
  in
    pieces ++ list'


intoChunks :: (Char -> Bool) -> S.ShortText -> [S.ShortText]
intoChunks _ piece | S.null piece = []
intoChunks predicate piece =
  let
    (chunk,remainder) = S.break predicate piece
    remainder' = S.drop 1 remainder
  in
    if S.null chunk
        then if S.null remainder
            then []
            else S.empty : intoChunks predicate remainder'
        else if S.null remainder
            then chunk : []
            else chunk : S.empty : intoChunks predicate remainder'
