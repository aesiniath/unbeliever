{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- As currently implemented this module, in conjunction with
-- Core.Text, is the opposite of efficient. The idea right now is to
-- experiment with the surface API. If it stabilizes, then the fact
-- that our string objects are already in UTF-8 will make for a very
-- efficient emitter.
--

{- |
Encoding and decoding UTF-8 JSON content.

This module is a thin wrapper around the most excellent __aeson__ library,
which has rich and powerful facilities for encoding Haskell types into JSON.

Quite often, however, you find yourself having to create a Haskell type /just/
to read some JSON coming from an external web service or API. This can be
challenging when the source of the JSON is complex or varying its schema over
time. For ease of exploration this module simply defines an easy to use
intermediate type representing JSON as a format.

Often you'll be working with literals directly in your code. While you can
write:

@
    j = 'JsonObject' ('intoMap' [('JsonKey' "answer", 'JsonNumber' 42)])
@

and it would be correct, enabling:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
\{\-\# LANGUAGE OverloadedLists \#\-\}
@

allows you to write:

@
    j = 'JsonObject' [("answer", 42)]
@

which you is somewhat less cumbersome in declaration-heavy code. You're
certainly welcome to use the constructors if you find it makes for more
readable code or if you need the type annotations.
-}
module Core.Encoding.Json (
    -- * Encoding and Decoding
    encodeToUTF8,
    decodeFromUTF8,
    JsonValue (..),
    JsonKey (..),

    -- * Syntax highlighting
    JsonToken (..),
    colourizeJson,
    prettyKey,
    prettyValue,
) where

import Core.Data.Structures (Key, Map, fromMap, intoMap)
import Core.Text.Bytes (Bytes, fromBytes, intoBytes)
import Core.Text.Rope (Rope, Textual, fromRope, intoRope)
import Core.Text.Utilities (
    AnsiColour,
    Render (Token, colourize, highlight),
    brightBlue,
    brightGrey,
    brightMagenta,
    dullBlue,
    dullCyan,
    dullGreen,
    dullYellow,
    pureGrey,
 )
import qualified Data.Aeson as Aeson
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Prettyprinter (
    Doc,
    Pretty (..),
    annotate,
    comma,
    dquote,
    group,
    hcat,
    indent,
    lbrace,
    lbracket,
    line,
    line',
    nest,
    punctuate,
    rbrace,
    rbracket,
    sep,
    unAnnotate,
    viaShow,
    vsep,
    (<+>),
 )

{- |
Given a JSON value, encode it to UTF-8 bytes

I know we're not /supposed/ to rely on types to document functions, but
really, this one does what it says on the tin.
-}
encodeToUTF8 :: JsonValue -> Bytes
encodeToUTF8 = intoBytes . Aeson.encode . intoAeson

{- |
Given an array of bytes, attempt to decode it as a JSON value.
-}
decodeFromUTF8 :: Bytes -> Maybe JsonValue
decodeFromUTF8 b =
    let x :: Maybe Aeson.Value
        x = Aeson.decodeStrict' (fromBytes b)
     in fmap fromAeson x

{- |
A JSON value.
-}
data JsonValue
    = JsonObject (Map JsonKey JsonValue)
    | JsonArray [JsonValue]
    | JsonString Rope
    | JsonNumber Scientific
    | JsonBool Bool
    | JsonNull
    deriving (Eq, Show, Generic)

--
-- Overloads so that Haskell code literals can be interpreted as JSON
-- values. Obviously these are a lot on the partial side, but what else are
-- you supposed to do? This is all Haskell gives us for getting at
-- literals.
--
instance IsString JsonValue where
    fromString :: String -> JsonValue
    fromString = JsonString . intoRope

instance Num JsonValue where
    fromInteger = JsonNumber . fromInteger
    (+) = error "Sorry, you can't add JsonValues"
    (-) = error "Sorry, you can't negate JsonValues"
    (*) = error "Sorry, you can't multiply JsonValues"
    abs = error "Sorry, not applicable for JsonValues"
    signum = error "Sorry, not applicable for JsonValues"

instance Fractional JsonValue where
    fromRational :: Rational -> JsonValue
    fromRational = JsonNumber . fromRational
    (/) = error "Sorry, you can't do division on JsonValues"

intoAeson :: JsonValue -> Aeson.Value
intoAeson value = case value of
    JsonObject xm ->
        let kvs = fromMap xm
            tvs = fmap (\(k, v) -> (fromRope (coerce k), intoAeson v)) kvs
            tvm :: HashMap T.Text Aeson.Value
            tvm = HashMap.fromList tvs
         in Aeson.Object tvm
    JsonArray xs ->
        let vs = fmap intoAeson xs
         in Aeson.Array (V.fromList vs)
    JsonString x -> Aeson.String (fromRope x)
    JsonNumber x -> Aeson.Number x
    JsonBool x -> Aeson.Bool x
    JsonNull -> Aeson.Null

{- |
Keys in a JSON object.
-}
newtype JsonKey
    = JsonKey Rope
    deriving (Eq, Show, Generic, IsString, Ord)

instance Hashable JsonKey

instance Key JsonKey

-- FIXME what is this instance?
instance Aeson.ToJSON Rope where
    toJSON text = Aeson.toJSON (fromRope text :: T.Text) -- BAD

instance Textual JsonKey where
    fromRope t = coerce t
    intoRope x = coerce x

fromAeson :: Aeson.Value -> JsonValue
fromAeson value = case value of
    Aeson.Object o ->
        let tvs = HashMap.toList o
            kvs = fmap (\(k, v) -> (JsonKey (intoRope k), fromAeson v)) tvs

            kvm :: Map JsonKey JsonValue
            kvm = intoMap kvs
         in JsonObject kvm
    Aeson.Array v -> JsonArray (fmap fromAeson (V.toList v))
    Aeson.String t -> JsonString (intoRope t)
    Aeson.Number n -> JsonNumber n
    Aeson.Bool x -> JsonBool x
    Aeson.Null -> JsonNull

--
-- Pretty printing
--

{- |
Support for pretty-printing JSON values with syntax highlighting using the
__prettyprinter__ library. To output a JSON structure to terminal
colourized with ANSI escape codes you can use the 'Render' instance:

@
    debug "j" (render j)
@

will get you:

@
23:46:04Z (00000.007) j =
{
    "answer": 42.0
}
@
-}
data JsonToken
    = SymbolToken
    | QuoteToken
    | KeyToken
    | StringToken
    | EscapeToken
    | NumberToken
    | BooleanToken
    | LiteralToken

instance Render JsonValue where
    type Token JsonValue = JsonToken
    colourize = colourizeJson
    highlight = prettyValue

instance Render JsonKey where
    type Token JsonKey = JsonToken
    colourize = colourizeJson
    highlight = prettyKey

instance Render Aeson.Value where
    type Token Aeson.Value = JsonToken
    colourize = colourizeJson
    highlight = prettyValue . fromAeson

--
--  Ugh. If you want to experiment with narrower output, then:
--
--            . layoutPretty (LayoutOptions {layoutPageWidth = AvailablePerLine 15 1.0}) . prettyValue
--

{- |
Used by the 'Render' instance to turn symbolic annotations into ANSI colours annotations.
If you're curious, the render pipeline looks like:

@
    render = 'intoText' . 'renderStrict' . 'reAnnotateS' 'colourize'
                . 'layoutPretty' 'defaultLayoutOptions' . 'prettyValue'
@
-}
colourizeJson :: JsonToken -> AnsiColour
colourizeJson token = case token of
    SymbolToken -> pureGrey
    QuoteToken -> brightGrey
    KeyToken -> brightBlue
    StringToken -> dullCyan
    EscapeToken -> dullYellow
    NumberToken -> dullGreen
    BooleanToken -> brightMagenta
    LiteralToken -> dullBlue

instance Pretty JsonKey where
    pretty = unAnnotate . prettyKey

prettyKey :: JsonKey -> Doc JsonToken
prettyKey (JsonKey t) =
    annotate QuoteToken dquote
        <> annotate KeyToken (pretty (fromRope t :: T.Text))
        <> annotate QuoteToken dquote

instance Pretty JsonValue where
    pretty = unAnnotate . prettyValue

prettyValue :: JsonValue -> Doc JsonToken
prettyValue value = case value of
    JsonObject xm ->
        let pairs = fromMap xm
            entries = fmap (\(k, v) -> (prettyKey k) <> annotate SymbolToken ":" <+> clear v (prettyValue v)) pairs

            clear v doc = case v of
                (JsonObject _) -> line <> doc
                (JsonArray _) -> group doc
                _ -> doc
         in if length entries == 0
                then annotate SymbolToken (lbrace <> rbrace)
                else annotate SymbolToken lbrace <> line <> indent 4 (vsep (punctuate (annotate SymbolToken comma) entries)) <> line <> annotate SymbolToken rbrace
    JsonArray xs ->
        let entries = fmap prettyValue xs
         in line'
                <> nest
                    4
                    ( annotate SymbolToken lbracket
                        <> line' -- first line not indented
                        <> sep (punctuate (annotate SymbolToken comma) entries)
                    )
                <> line'
                <> annotate SymbolToken rbracket
    JsonString x ->
        annotate QuoteToken dquote
            <> annotate StringToken (escapeText x)
            <> annotate QuoteToken dquote
    JsonNumber x -> annotate NumberToken (viaShow x)
    JsonBool x -> case x of
        True -> annotate BooleanToken "true"
        False -> annotate BooleanToken "false"
    JsonNull -> annotate LiteralToken "null"
{-# INLINEABLE prettyValue #-}

escapeText :: Rope -> Doc JsonToken
escapeText text =
    let t = fromRope text :: T.Text
        ts = T.split (== '"') t
        ds = fmap pretty ts
     in hcat (punctuate (annotate EscapeToken "\\\"") ds)
{-# INLINEABLE escapeText #-}
