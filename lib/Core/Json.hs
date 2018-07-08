{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Json
  ( encodeToUTF8
  , decodeFromUTF8
  ) where

import qualified Core.Text as Core
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import GHC.Generics
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

encodeToUTF8 :: JsonValue -> Core.Bytes
encodeToUTF8 = undefined

decodeFromUTF8 :: Core.Bytes -> JsonValue
decodeFromUTF8 = undefined

data JsonValue
  = JsonObject (HashMap JsonKey JsonValue)
  | JsonArray [JsonValue]
  | JsonString Core.Text
  | JsonNumber Scientific
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Read, Show, Generic)

newtype JsonKey = JsonKey Core.Text
  deriving (Eq, Show, Read, Generic)

instance Hashable JsonKey
instance Aeson.ToJSON JsonKey
instance Aeson.ToJSONKey JsonKey

instance Aeson.ToJSON Core.Text where
  toJSON = Aeson.toJSON . Core.intoText 


instance Aeson.FromJSON JsonKey
instance Aeson.FromJSONKey JsonKey
instance Aeson.FromJSON JsonValue

instance Aeson.FromJSON Core.Text where
    parseJSON = Aeson.parseJSON