{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}


{- |
Quite frequently you will find yourself needing to convert between a Haskell
data type and a textual representation of that type.

Note that /externalizing/ is not the same as /serializing/. If you have more
complex (ie rich types or nested) data structures then a simple text string
will not be sufficient to represent it accurately. Serializing is focused on
both performance encoding and decoding, and efficiency of the representation
when transmitted over the wire.

Of course, the obvious benefits of efficiency didn't stop the entire computer
industry from near universal adoption of JSON as an interchange format. There
is, perhaps, no hope for us.
-}
module Core.Data.External (
    -- * Conversions
    Externalize (intoExternal, parseExternal),
) where

import Core.Text.Rope

import Data.Int (Int64)
import Text.Read (readMaybe)

{- |
Convert between different representations of a data type.


There is a general implementatation that goes though 'Show' and 'Read' via
'String' but if you know you have a direct way to render or parse a type into
a sequence of characters then you can offer an instance of 'Externalize' which
does so more efficiently.

@since 0.3.4
-}
class Externalize a where
    intoExternal :: a -> Rope
    parseExternal :: Rope -> Maybe a

--
-- We use this general instance here rather than as a super class constraint
-- for Externalize so as to allow us to have things that can be externalized
-- without necessarily needing those two instances. Most things have Show, but
-- not everything, an many many things haven't bothered with Read.
--

instance {-# OVERLAPPABLE #-} (Read a, Show a) => Externalize a where
    intoExternal = intoRope . show
    parseExternal = readMaybe . fromRope

instance Externalize Int where
    intoExternal = intoRope . show
    parseExternal = readMaybe . fromRope

instance Externalize Int64 where
    intoExternal = intoRope . show
    parseExternal = readMaybe . fromRope
