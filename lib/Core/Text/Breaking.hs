{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module, hidden from Haddock
module Core.Text.Breaking
    (
      breakPieces
    , intoPieces
    , intoChunks
    )
where

import Data.Foldable (foldr)
import Data.List (uncons)
import qualified Data.Text.Short as S (ShortText, null, drop, break)

import Core.Text.Rope

{-|
Break a Rope into pieces whereever the given predicate function returns
@True@. If found, that character will not be included on either side. Empty
runs, however, *will* be preserved.
-}
breakPieces :: (Char -> Bool) -> Rope -> [Rope]
breakPieces predicate text =
  let
    x = unRope text
    result = foldr (intoPieces predicate) [] x
  in
    result

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

--
-- λ> S.break isSpace "a d"
-- ("a"," d")
--
-- λ> S.break isSpace " and"
-- (""," and")
--
-- λ> S.break isSpace "and "
-- ("and"," ")
--
-- λ> S.break isSpace ""
-- ("","")
--
-- λ> S.break isSpace " "
-- (""," ")
--
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
