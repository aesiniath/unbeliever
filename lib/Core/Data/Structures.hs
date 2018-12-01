{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Data.Structures
(
      {-* Map type -}
      Map
    , Key
    , emptyMap
    , singletonMap
    , insertKeyValue
    , lookupKeyValue

      {-* Conversions -}
    , Dictionary(fromMap, intoMap)

      {-* Internals -}
    , unMap
    , containsMap
)
where

import Data.Foldable (Foldable(..))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Unordered
import qualified Data.Map.Strict as Containers
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)
import qualified GHC.Exts as Exts (IsList(..))

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

instance Foldable (Map κ) where
    foldr f start (Map p) = Unordered.foldr f start p
    null (Map p) = Unordered.null p
    length (Map p) = Unordered.size p

emptyMap :: Map κ ν
emptyMap = Map (Unordered.empty)

singletonMap :: Key κ => κ -> ν -> Map κ ν
singletonMap k v = Map (Unordered.singleton k v)

insertKeyValue :: Key κ => κ -> ν -> Map κ ν -> Map κ ν
insertKeyValue k v (Map p) = Map (Unordered.insert k v p)

lookupKeyValue :: Key κ => κ -> Map κ ν -> Maybe ν
lookupKeyValue k (Map p) = Unordered.lookup k p

containsMap :: Key κ => κ -> Map κ ν -> Bool
containsMap k (Map p) = Unordered.member k p

{-|
-}
instance Key κ => Semigroup (Map κ ν) where
    (<>) (Map p1) (Map p2) = Map (Unordered.union p1 p2)

instance Key κ => Monoid (Map κ ν) where
    mempty = emptyMap
    mappend = (<>)

instance Key κ => Exts.IsList (Map κ ν) where
    type Item (Map κ ν) = (κ, ν)
    fromList pairs = Map (Unordered.fromList pairs)
    toList (Map p) = Unordered.toList p

type Set κ = Map κ ()

{-|
Types that represent key/value pairs that can be converted to 'Map's.
Haskell's ecosystem has several such. This typeclass provides an adaptor to
get between them. It also allows you to serialize out to an association
list.

For example, to convert a 'Map' to an \"association list\" of key/value
pairs, use 'fromMap':

@
    answers :: 'Map' 'Rope' 'Int'
    answers = 'singletonMap' \"Life, The Universe, and Everything\" 42

    list :: [('Rope','Int')]
    list = 'fromMap' answers
@

Instances are provided for __containers__'s 'Data.Map.Strict.Map' and
__unordered-containers__'s 'Data.HashMap.Strict.HashMap' in addition to the
instance for @[(κ,ν)]@ lists shown above.
-}
--
-- Getting an instance for [(κ,ν)] was very difficult. The approach
-- implemented below was suggested by Xia Li-yao, @Lysxia was to use
-- type families.
--
-- >   "Maybe you can change your type class to be indexed by the fully
-- >   applied dictionary type, instead of a type constructor * -> * -> *"
--
-- https://stackoverflow.com/questions/53554687/list-instances-for-higher-kinded-types/53556313
--
-- Many thanks for an elegant solution to the problem.
--
class Dictionary α where
    type K α :: *
    type V α :: *
    fromMap :: Map (K α) (V α) -> α
    intoMap :: α -> Map (K α) (V α)

instance Key κ => Dictionary (Unordered.HashMap κ ν)  where
    type K (Unordered.HashMap κ ν) = κ
    type V (Unordered.HashMap κ ν) = ν
    fromMap (Map p) = p
    intoMap p = Map p

instance Key κ => Dictionary (Containers.Map κ ν) where
    type K (Containers.Map κ ν) = κ
    type V (Containers.Map κ ν) = ν
    fromMap (Map p) = Containers.fromList (Unordered.toList p)
    intoMap o = Map (Unordered.fromList (Containers.toList o))

instance Key κ => Dictionary [(κ,ν)] where
    type K [(κ,ν)] = κ
    type V [(κ,ν)] = ν
    fromMap (Map p) = Unordered.toList p
    intoMap kvs = Map (Unordered.fromList kvs)
