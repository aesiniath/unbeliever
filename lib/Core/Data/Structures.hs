{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Data.Structures
(
      {-* Map type -}
      Map
    , Key
    , size1
    , empty1
    , singleton1
    , insert1
    , lookup1
    , fromList1
    , intoList1
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
import qualified Data.Map.Strict as Containers
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)
import GHC.Exts (IsList(..))

import Core.Text.Rope (Rope)
import Core.Text.Bytes (Bytes)

newtype Map κ ν = Map (Unordered.HashMap κ ν)
    deriving (Show, Eq)

unMap :: Map κ ν -> Unordered.HashMap κ ν
unMap (Map p) = p
{-# INLINE unMap #-}

class (Hashable κ, Ord κ) => Key κ

instance Key String 
instance Key Rope 
instance Key Bytes 
instance Key T.Text
instance Key U.Text
instance Key Char

size1 :: Map κ ν -> Int
size1 (Map p) = Unordered.size p

empty1 :: Map κ ν
empty1 = Map (Unordered.empty)

singleton1 :: Key κ => κ -> ν -> Map κ ν
singleton1 k v = Map (Unordered.singleton k v)

insert1 :: Key κ => κ -> ν -> Map κ ν -> Map κ ν
insert1 k v (Map p) = Map (Unordered.insert k v p)

lookup1 :: Key κ => κ -> Map κ ν -> Maybe ν
lookup1 k (Map p) = Unordered.lookup k p

contains1 :: Key κ => κ -> Map κ ν -> Bool
contains1 k (Map p) = Unordered.member k p

fromList1 :: Key κ => [(κ,ν)] -> Map κ ν
fromList1 pairs = Map (Unordered.fromList pairs)

intoList1 :: Key κ => Map κ ν -> [(κ,ν)]
intoList1 (Map p) = Unordered.toList p

instance Key κ => Semigroup (Map κ ν) where
    (<>) (Map p1) (Map p2) = Map (Unordered.union p1 p2)

instance Key κ => Monoid (Map κ ν) where
    mempty = empty1
    mappend = (<>)

instance Key κ => IsList (Map κ ν) where
    type Item (Map κ ν) = (κ, ν)
    fromList = fromList1
    toList (Map p) = toList p

type Set κ = Map κ ()

class Dictionary α where
    fromMap :: Key κ => Map κ ν -> α κ ν
    intoMap :: Key κ => α κ ν -> Map κ ν

instance Dictionary Unordered.HashMap where
    fromMap (Map p) = p
    intoMap p = Map p

instance Dictionary Containers.Map where
    fromMap (Map p) = Containers.fromList (Unordered.toList p)
    intoMap o = Map (Unordered.fromList (Containers.toList o))

