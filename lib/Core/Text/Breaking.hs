{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- This is an Internal module, hidden from Haddock
module Core.Text.Breaking
    ( breakWords
    , breakLines
    , breakPieces
    , intoPieces
    , intoChunks
    )
where

import Data.Char (isSpace)
import Data.Foldable (foldr)
import Data.List (uncons)
import qualified Data.Text.Short as S (ShortText, null, break, uncons,empty)

import Core.Text.Rope

{-|
Split a passage of text into a list of words. A line is broken wherever
there is one or more whitespace characters, as defined by "Data.Char"'s
'Data.Char.isSpace'.

Examples:

@
λ> __breakWords \"This is a test\"__
[\"This\",\"is\",\"a\",\"test\"]
λ> __breakWords (\"St\" <> \"op and \" <> \"go left\")__
[\"Stop\",\"and\",\"go\",\"left\"]
λ> __breakWords emptyRope__
[]
@
-}
breakWords :: Rope -> [Rope]
breakWords = filter (not . nullRope) . breakPieces isSpace

{-|
Split a paragraph of text into a list of its individual lines. The
paragraph will be broken wherever there is a @'\n'@ character.

Blank lines will be preserved. Note that as a special case you do /not/ get
a blank entry at the end of the a list of newline terminated strings.

@
λ> __breakLines \"Hello\\n\\nWorld\\n\"__
[\"Hello\",\"\",\"World\"]
@
-}
breakLines :: Rope -> [Rope]
breakLines text =
  let
    result = breakPieces isNewline text
    n = length result - 1
    (fore,aft) = splitAt n result
  in case result of
    [] -> []
    [p] -> [p]
    _ -> if aft == [""]
        then fore
        else result

isNewline :: Char -> Bool
isNewline c = c == '\n'

{-|
Break a Rope into pieces whereever the given predicate function returns
@True@. If found, that character will not be included on either side. Empty
runs, however, *will* be preserved.
-}
breakPieces :: (Char -> Bool) -> Rope -> [Rope]
breakPieces predicate text =
  let
    x = unRope text
    (final,result) = foldr (intoPieces predicate) (Nothing,[]) x
  in
    case final of
       Nothing -> result
       Just piece -> intoRope piece : result

{-
Was the previous piece a match, or are we in the middle of a run of
characters? If we were, then join the previous run to the current piece
before processing into chunks.
-}
-- now for right fold
intoPieces :: (Char -> Bool) -> S.ShortText -> (Maybe S.ShortText,[Rope]) -> (Maybe S.ShortText,[Rope])
intoPieces predicate piece (stream,list) =
  let
    piece' = case stream of
        Nothing -> piece
        Just previous -> piece <> previous       -- more rope, less text?

    pieces = intoChunks predicate piece'
  in
    case uncons pieces of
        Nothing -> (Nothing,list)
        Just (text,remainder) -> (Just (fromRope text),remainder ++ list)

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

{-
This was more easily expressed as 

  let
    remainder' = S.drop 1 remainder
  in
    if remainder == " "

for the case when we were breaking on spaces. But generalized to a predicate
we have to strip off the leading character and test that its the only character;
this is cheaper than S.length etc.
-}
intoChunks :: (Char -> Bool) -> S.ShortText -> [Rope]
intoChunks _ piece | S.null piece = []
intoChunks predicate piece =
  let
    (chunk,remainder) = S.break predicate piece

    -- Handle the special case that a trailing " " (generalized to predicate)
    -- is the only character left.
    (trailing,remainder') = case S.uncons remainder of
        Nothing -> (False,S.empty)
        Just (c,remaining) -> if S.null remaining
            then (predicate c,S.empty)
            else (False,remaining)
  in
    if trailing
        then intoRope chunk : emptyRope : []
        else intoRope chunk : intoChunks predicate remainder'
