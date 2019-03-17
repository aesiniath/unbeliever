{-# LANGUAGE OverloadedStrings #-}

module Core.Text.Breaking where

import qualified Data.Text.Short as S
import Data.Foldable (foldl')

import Core.Text.Rope

-- for left fold
intoPieces :: (Char -> Bool) -> [S.ShortText] -> S.ShortText -> [S.ShortText] -- maybe Rope at this point?
intoPieces predicate [] piece =
  let
    pieces = intoChunks predicate piece
  in
    pieces
intoPieces predicate list piece =
  let
    previous = last list
    remainder = init list

    (list',piece') = if S.null previous
            then (list,piece)
            else (remainder,previous <> piece)

    pieces = intoChunks predicate piece'
  in
    list' ++ pieces


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
