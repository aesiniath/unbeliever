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
    , containsKey
    , lookupKeyValue

      {-* Conversions -}
    , Dictionary(fromMap, intoMap)

      {-* Set type -}
    , Set
    , emptySet
    , singletonSet
    , insertElement
    , containsElement
    , Collection(fromSet, intoSet)

      {-* Internals -}
    , unMap
    , unSet
)
where

import Data.Foldable (Foldable(..))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as OrdMap
import qualified Data.Set as OrdSet
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as U (Text)
import qualified GHC.Exts as Exts (IsList(..))

import Core.Text.Rope (Rope)
import Core.Text.Bytes (Bytes)

-- Naming convention used throughout this file is (Thing u) where u is the
-- underlying structure [from unordered-containers] wrapped in the Thing
-- newtype. Leaves p for our Map and s for our Set in tests.

newtype Map κ ν = Map (HashMap.HashMap κ ν)
    deriving (Show, Eq)

unMap :: Map κ ν -> HashMap.HashMap κ ν
unMap (Map u) = u
{-# INLINE unMap #-}

class (Hashable κ, Ord κ) => Key κ

instance Key String 
instance Key Rope 
instance Key Bytes 
instance Key T.Text
instance Key U.Text
instance Key Char
instance Key Int

instance Foldable (Map κ) where
    foldr f start (Map u) = HashMap.foldr f start u
    null (Map u) = HashMap.null u
    length (Map u) = HashMap.size u

emptyMap :: Map κ ν
emptyMap = Map (HashMap.empty)

singletonMap :: Key κ => κ -> ν -> Map κ ν
singletonMap k v = Map (HashMap.singleton k v)

insertKeyValue :: Key κ => κ -> ν -> Map κ ν -> Map κ ν
insertKeyValue k v (Map u) = Map (HashMap.insert k v u)

lookupKeyValue :: Key κ => κ -> Map κ ν -> Maybe ν
lookupKeyValue k (Map u) = HashMap.lookup k u

containsKey :: Key κ => κ -> Map κ ν -> Bool
containsKey k (Map u) = HashMap.member k u

{-|
-}
instance Key κ => Semigroup (Map κ ν) where
    (<>) (Map u1) (Map u2) = Map (HashMap.union u1 u2)

instance Key κ => Monoid (Map κ ν) where
    mempty = emptyMap
    mappend = (<>)

instance Key κ => Exts.IsList (Map κ ν) where
    type Item (Map κ ν) = (κ, ν)
    fromList pairs = Map (HashMap.fromList pairs)
    toList (Map u) = HashMap.toList u

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

instance Key κ => Dictionary (Map κ ν) where
    type K (Map κ ν) = κ
    type V (Map κ ν) = ν
    fromMap = id
    intoMap = id

{-| from "Data.HashMap.Strict" -}
instance Key κ => Dictionary (HashMap.HashMap κ ν) where
    type K (HashMap.HashMap κ ν) = κ
    type V (HashMap.HashMap κ ν) = ν
    fromMap (Map u) = u
    intoMap u = Map u

{-| from "Data.Map.Strict" -}
instance Key κ => Dictionary (OrdMap.Map κ ν) where
    type K (OrdMap.Map κ ν) = κ
    type V (OrdMap.Map κ ν) = ν
    fromMap (Map u) = OrdMap.fromList (HashMap.toList u)
    intoMap u = Map (HashMap.fromList (OrdMap.toList u))

instance Key κ => Dictionary [(κ,ν)] where
    type K [(κ,ν)] = κ
    type V [(κ,ν)] = ν
    fromMap (Map u) = HashMap.toList u
    intoMap kvs = Map (HashMap.fromList kvs)


newtype Set ε = Set (HashSet.HashSet ε)
    deriving (Show, Eq)

unSet :: Set ε -> HashSet.HashSet ε
unSet (Set u) = u
{-# INLINE unSet #-}

instance Foldable Set where
    foldr f utart (Set u) = HashSet.foldr f utart u
    null (Set u) = HashSet.null u
    length (Set u) = HashSet.size u

instance Key ε => Semigroup (Set ε) where
    (<>) (Set u1) (Set u2) = Set (HashSet.union u1 u2)

instance Key ε => Monoid (Set ε) where
    mempty = emptySet
    mappend = (<>)

emptySet :: Key ε => Set ε
emptySet = Set (HashSet.empty)

singletonSet :: Key ε => ε -> Set ε
singletonSet e = Set (HashSet.singleton e)

insertElement :: Key ε => ε -> Set ε -> Set ε
insertElement e (Set u) = Set (HashSet.insert e u)

containsElement :: Key ε => ε -> Set ε -> Bool
containsElement e (Set u) = HashSet.member e u

class Collection α where
    type E α :: *
    fromSet :: Set (E α) -> α
    intoSet :: α -> Set (E α)

instance Key ε => Collection (Set ε) where
    type E (Set ε) = ε
    fromSet = id
    intoSet = id

instance Key ε => Collection (HashSet.HashSet ε) where
    type E (HashSet.HashSet ε) = ε
    fromSet (Set u) = u
    intoSet u = Set u

instance Key ε => Collection (OrdSet.Set ε) where
    type E (OrdSet.Set ε) = ε
    fromSet (Set u) = HashSet.foldr OrdSet.insert OrdSet.empty u
    intoSet u = Set (OrdSet.foldr HashSet.insert HashSet.empty u)

instance Key ε => Collection [ε] where
    type E [ε] = ε
    fromSet (Set u) = OrdSet.toList (HashSet.foldr OrdSet.insert OrdSet.empty u)
    intoSet es = Set (HashSet.fromList es)
