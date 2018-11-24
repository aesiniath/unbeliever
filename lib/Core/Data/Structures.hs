{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Data.Structures
(
      {-* Map type -}
      Map
    , Key
    , insert1
    , lookup1
    , Dictionary(fromMap, intoMap)

      {-* Internals -}
    , unMap
    , contains1
)
where

{-
Naming convention: the trouble is `lookupMap` etc collides with the
convetion inherent in `concatMap`. Tried `lookupIn`, `lookupOf`,
`lookupInMap`, etc. `lookupOne` wasn't bad ("lookup the one in"). Settled
on `lookup1` because brevity.
-}

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

empty1  :: Map κ ν
empty1 = Map (Unordered.empty)

singleton1  :: Key κ => κ -> ν -> Map κ ν
singleton1 k v = Map (Unordered.singleton)

insert1  :: Key κ => κ -> ν -> Map κ ν -> Map κ ν
insert1 k v (Map p) = Map (Unordered.insert k v p)

lookup1 :: Key κ => κ -> Map κ ν -> Maybe ν
lookup1 k (Map p) = Unordered.lookup k p

contains1 :: Key κ => κ -> Map κ ν -> Bool
contains1 k (Map p) = Unordered.member k p


type Set κ = Map κ ()

--class Dictionary α where
class Dictionary α κ ν where
    fromMap :: Map κ ν -> α κ ν
    intoMap :: α κ ν -> Map κ ν

instance Dictionary Unordered.HashMap κ ν where
    fromMap (Map p) = p
    intoMap p = Map p



