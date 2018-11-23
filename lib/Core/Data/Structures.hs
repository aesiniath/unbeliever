{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Data.Structures
(
      {-* Map type -}
      Map
    , Key
    , insertAt
    , lookupAt
    , Set
    , Dictionary(fromMap, intoMap)

      {-* Internals -}
    , unMap
    , containsAt
)
where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Unordered
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)

import Core.Text.Rope (Rope)
import Core.Text.Bytes (Bytes)

newtype Map κ ν = Map (Unordered.HashMap κ ν)

unMap :: Map κ ν -> Unordered.HashMap κ ν
unMap (Map p) = p
{-# INLINE unMap #-}

class (Hashable κ, Ord κ) => Key κ

instance Key String 
instance Key Rope 
instance Key Bytes 

insertAt  :: Key κ => κ -> ν -> Map κ ν -> Map κ ν
insertAt k v (Map p) = Map (Unordered.insert k v p)

lookupAt :: Key κ => κ -> Map κ ν -> Maybe ν
lookupAt k (Map p) = Unordered.lookup k p

containsAt :: Key κ => κ -> Map κ ν -> Bool
containsAt k (Map p) = Unordered.member k p


type Set κ = Map κ ()

--class Dictionary α where
class Dictionary α κ ν where
    fromMap :: Map κ ν -> α κ ν
    intoMap :: α κ ν -> Map κ ν

instance Dictionary Unordered.HashMap κ ν where
    fromMap (Map p) = p
    intoMap p = Map p



