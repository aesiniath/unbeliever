{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module, hidden from Haddock
module Core.Text.Breaking
    (
      intoPieces
    , intoChunks
    )
where

import Data.Foldable (foldl')
import Data.List (uncons)
import qualified Data.Text.Short as S

import Core.Text.Rope

{-
Was the previous piece a match, or are we in the middle of a run of
characters? If we were, then join the previous run to the current piece
before processing into chunks.
-}
-- now for right fold
intoPieces :: (Char -> Bool) -> S.ShortText -> [Rope] -> [Rope]
intoPieces predicate piece list =
  let
    (piece',list') = case uncons list of
        Nothing -> (piece,[])
        Just (previous,remainder) -> if nullRope previous
            then (piece,list)
            else
                (piece <> fromRope previous,remainder)

    pieces = intoChunks predicate piece'
  in
    pieces ++ list'

intoChunks :: (Char -> Bool) -> S.ShortText -> [Rope]
intoChunks _ piece | S.null piece = []
intoChunks predicate piece =
  let
    (chunk,remainder) = S.break predicate piece
    remainder' = S.drop 1 remainder
  in
    if S.null chunk
        then if S.null remainder
            then []
            else emptyRope : intoChunks predicate remainder'
        else if S.null remainder
            then intoRope chunk : []
            else intoRope chunk : emptyRope : intoChunks predicate remainder'
