{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    As currently implemented this module, in conjunction with
    Core.Text, is the opposite of efficient. The idea right now is to
    experiment with the surface API. If it stabilizes, then the fact
    that our string objects are already in UTF-8 will make for a very
    efficient emitter.
-}

module Core.Json
    ( encodeToUTF8
    , decodeFromUTF8
    , JsonValue(..)
    , JsonKey(..)
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Coerce
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

import Core.Text (Text(UTF8), Bytes(StrictBytes), Textual, intoText, fromText)
import Core.Render (Render, render)

encodeToUTF8 :: JsonValue -> Bytes
encodeToUTF8 = StrictBytes . S.concat . L.toChunks . Aeson.encode . intoAeson

decodeFromUTF8 :: Bytes -> Maybe JsonValue
decodeFromUTF8 (StrictBytes b') =
  let
    x :: Maybe Aeson.Value
    x = Aeson.decodeStrict' b'
  in
    fmap fromAeson x

data JsonValue
    = JsonObject (HashMap JsonKey JsonValue)
    | JsonArray [JsonValue]
    | JsonString Text
    | JsonNumber Scientific
    | JsonBool Bool
    | JsonNull
    deriving (Eq, Read, Show, Generic)

intoAeson :: JsonValue -> Aeson.Value
intoAeson value = case value of
    JsonObject xm ->
        let
            kvs = HashMap.toList xm
            tvs = fmap (\(k, v) -> (fromText (coerce k), intoAeson v)) kvs
            tvm :: HashMap T.Text Aeson.Value
            tvm = HashMap.fromList tvs
        in
            Aeson.Object tvm

    JsonArray xs ->
        let
            vs = fmap intoAeson xs
        in
            Aeson.Array (V.fromList vs)

    JsonString x -> Aeson.String (fromText x)
    JsonNumber x -> Aeson.Number x
    JsonBool x -> Aeson.Bool x
    JsonNull -> Aeson.Null


newtype JsonKey
    = JsonKey Text
    deriving (Eq, Show, Read, Generic, IsString)

instance Hashable JsonKey

instance Render JsonKey where
    render (JsonKey t) = intoText (T.concat ["\"", fromText t, "\""])

instance Aeson.ToJSON Text where
    toJSON b' = Aeson.toJSON (fromText b' :: T.Text) -- BAD

instance Textual JsonKey where
    fromText t = coerce t
    intoText x = coerce x


fromAeson :: Aeson.Value -> JsonValue
fromAeson value = case value of
    Aeson.Object o ->
        let
            tvs = HashMap.toList o
            kvs = fmap (\(k, v) -> (JsonKey (intoText k), fromAeson v)) tvs

            kvm :: HashMap JsonKey JsonValue
            kvm = HashMap.fromList kvs
        in
            JsonObject kvm

    Aeson.Array v -> JsonArray (fmap fromAeson (V.toList v))
    Aeson.String t -> JsonString (intoText t)
    Aeson.Number n -> JsonNumber n
    Aeson.Bool x -> JsonBool x
    Aeson.Null -> JsonNull


instance Render JsonValue where
    render = renderValue

renderValue :: JsonValue -> Text
renderValue value = case value of
    JsonObject xm ->
        let
            kvs = HashMap.toList xm
            pairs = fmap (\(k, v) -> (render k, render v)) kvs
            entries = fmap (\(k, v) -> S.concat ["    ", fromText k, ": ", fromText v]) pairs
        in
            if length entries == 0
                then UTF8 "{}"
                else UTF8 $ S.concat
                    [ "{\n"
                    , C.intercalate ",\n" entries
                    , "\n}\n"
                    ]

    JsonArray xs ->
        let
            ts = fmap render xs
            entries = fmap fromText ts
        in
            UTF8 $ S.concat
                    [ "["
                    , C.intercalate ", " entries
                    , "]\n"
                    ]

    JsonString x -> UTF8 (S.concat ["\"", fromText x, "\""])
    JsonNumber x -> UTF8 (C.pack (show x)) -- FIXME
    JsonBool x -> case x of
        True -> UTF8 "true"
        False -> UTF8 "false"
    JsonNull -> UTF8 "null"
{-# INLINEABLE renderValue #-}

