{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Json
    ( encodeToUTF8
    , decodeFromUTF8
    , JsonValue(..)
    , JsonKey(..)
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

import qualified Core.Text as Core
import qualified Core.Render as Core

encodeToUTF8 :: JsonValue -> Core.Bytes
encodeToUTF8 = Core.StrictBytes . S.concat . L.toChunks . Aeson.encode . intoAeson

decodeFromUTF8 :: Core.Bytes -> Maybe JsonValue
decodeFromUTF8 (Core.StrictBytes b') =
  let
    x :: Maybe Aeson.Value
    x = Aeson.decodeStrict' b'
  in
    fmap fromAeson x

data JsonValue
    = JsonObject (HashMap JsonKey JsonValue)
    | JsonArray [JsonValue]
    | JsonString Core.Text
    | JsonNumber Scientific
    | JsonBool Bool
    | JsonNull
    deriving (Eq, Read, Show, Generic)

intoAeson :: JsonValue -> Aeson.Value
intoAeson value = case value of
    JsonObject xm ->
        let
            kvs = HashMap.toList xm
            tvs = fmap (\(k, v) -> (Core.fromText (unJsonKey k), intoAeson v)) kvs
            tvm :: HashMap T.Text Aeson.Value
            tvm = HashMap.fromList tvs
        in
            Aeson.Object tvm

    JsonArray xs ->
        let
            vs = fmap intoAeson xs
        in
            Aeson.Array (V.fromList vs)

    JsonString x -> Aeson.String (Core.fromText x)
    JsonNumber x -> Aeson.Number x
    JsonBool x -> Aeson.Bool x
    JsonNull -> Aeson.Null


newtype JsonKey
    = JsonKey Core.Text
    deriving (Eq, Show, Read, Generic, IsString)

unJsonKey :: JsonKey -> Core.Text
unJsonKey (JsonKey t) = t

instance Hashable JsonKey

instance Core.Render JsonKey where
    render (JsonKey t) = Core.intoText (T.concat ["\"", Core.fromText t, "\""])

instance Aeson.ToJSON Core.Text where
    toJSON (Core.UTF8 b') = error "No coding"
    toJSON (Core.StrictText t) = Aeson.toJSON t

instance Core.Unicode JsonKey where
    fromText t = Core.fromText t
    intoText x = Core.intoText x


fromAeson :: Aeson.Value -> JsonValue
fromAeson value = case value of
    Aeson.Object o ->
        let
            tvs = HashMap.toList o
            kvs = fmap (\(k, v) -> (JsonKey (Core.intoText k), fromAeson v)) tvs

            kvm :: HashMap JsonKey JsonValue
            kvm = HashMap.fromList kvs
        in
            JsonObject kvm

    Aeson.Array v -> JsonArray (fmap fromAeson (V.toList v))
    Aeson.String t -> JsonString (Core.intoText t)
    Aeson.Number n -> JsonNumber n
    Aeson.Bool x -> JsonBool x
    Aeson.Null -> JsonNull
