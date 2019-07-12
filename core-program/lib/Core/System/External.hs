{-# OPTIONS_HADDOCK not-home #-}

--
-- | Re-exports of dependencies from various external libraries.
--
module Core.System.External
    ( {-* Time -}
      {-** from Chrono.TimeStamp -}
      {-| Re-exported from "Chrono.TimeStamp" in __chronologique__: -}
      TimeStamp(..)
    , getCurrentTimeNanoseconds
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)

