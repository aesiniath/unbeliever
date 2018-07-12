{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Json
    ( encodeToUTF8
    , decodeFromUTF8
    ) where

import qualified Core.Text as Core
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.String (IsString)
import GHC.Generics

encodeToUTF8 :: JsonValue -> Core.Bytes
encodeToUTF8 = Core.StrictBytes . S.concat . L.toChunks . Aeson.encode

decodeFromUTF8 :: Core.Bytes -> Maybe JsonValue
decodeFromUTF8 (Core.StrictBytes b') = Aeson.decodeStrict' b'

data JsonValue
    = JsonObject (HashMap JsonKey JsonValue)
    | JsonArray [JsonValue]
    | JsonString Core.Text
    | JsonNumber Scientific
    | JsonBool Bool
    | JsonNull
    deriving (Eq, Read, Show, Generic)

newtype JsonKey =
    JsonKey Core.Text
    deriving (Eq, Show, Read, Generic, IsString)

instance Hashable JsonKey

instance Aeson.ToJSON JsonKey

instance Aeson.ToJSONKey JsonKey

instance Aeson.ToJSON JsonValue

instance Aeson.ToJSON Core.Text where
    toJSON = Aeson.toJSON . Core.intoText

instance Aeson.FromJSON JsonKey

instance Aeson.FromJSONKey JsonKey

instance Aeson.FromJSON JsonValue

instance Aeson.FromJSON Core.Text where
    parseJSON = Aeson.parseJSON
