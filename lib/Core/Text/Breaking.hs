{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Short as S

import Core.Text.Rope

intoChunks :: (Char -> Bool) -> S.ShortText -> [S.ShortText]
intoChunks _ piece | S.null piece = []
intoChunks predicate piece =
  let
    (chunk,remainder') = S.break predicate piece
    remainder = S.drop 1 remainder'
  in
    chunk : intoChunks predicate remainder
