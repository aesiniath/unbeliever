{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}


{- |
Quite frequently you will find yourself needing to convert between a rich
semantic Haskell data type and a textual representation of that type which we
call the /external/ representation of a value.

Note that /externalizing/ is not quite the same as /serializing/. If you have
more complex (ie rich types or nested) data structures then a simple text
string will probably not be sufficient to convey sufficient information to
represent it accurately. Serializing is focused on both performance encoding
and decoding, and efficiency of the representation when transmitted over the
wire. Of course, the obvious benefits of efficiency didn't stop the entire
computer industry from near universal adoption of JSON as an interchange
format, so there is, perhaps, no hope for us.

You can, however, regain some of your sanity by ensuring that the individual
fields of a larger structure are safe, and that's where the externalizing
machinery in this module comes in.

If you have read this far and think we are describing 'Show' or @toString@ you
are correct, but at the level of primative and simple types we are providing
the ability to marshall them to a clean UTF-8 representation and to unmarshall
them back into Haskell values again. This external representation of the value
is authoriative and is meant to be re-readable even in the face of changing
implemetations on the program side.
-}
module Core.Encoding.External (
    -- * Conversions
    Externalize (formatExternal, parseExternal),
) where

import Core.Text.Rope

import Data.Int (Int64)
import Text.Read (readMaybe)

{- |
Convert between the internal Haskell representation of a data type and an
external, textual form suitable for visualization, onward transmission, or
archival storage.

It is expected that a valid instance of 'Externalize' allows you to round-trip
through it:

>>> formatExternal (42 :: Int))
"42"

>>> fromJust (parseExternal "42") :: Int
42

with the usual caveat about needing to ensure you've given enough information
to the type-checker to know which instance you're asking for.

There is a general implementatation that goes though 'Show' and 'Read' via
'String' but if you know you have a direct way to render or parse a type into
a sequence of characters then you can offer an instance of 'Externalize' which
does so more efficiently.

@since 0.3.4
-}
class Externalize a where
    formatExternal :: a -> Rope
    parseExternal :: Rope -> Maybe a

--
-- We use this general instance here rather than as a super class constraint
-- for Externalize so as to allow us to have things that can be externalized
-- without necessarily needing those two instances. Most things have Show, but
-- not everything, as many many types haven't bothered with Read.
--

instance {-# OVERLAPPABLE #-} (Read a, Show a) => Externalize a where
    formatExternal = intoRope . show
    parseExternal = readMaybe . fromRope

instance Externalize Int where
    formatExternal = intoRope . show
    parseExternal = readMaybe . fromRope

instance Externalize Int64 where
    formatExternal = intoRope . show
    parseExternal = readMaybe . fromRope
