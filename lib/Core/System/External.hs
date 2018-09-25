{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

--
-- | Re-exports of Haskell base and GHC system libraries.
--
module Core.System.External
    ( {-* Input/Output -}
      {-** from Control.Monad.IO.Class -}
      {-| Reexported from "Control.Monad.IO.Class" in __base__: -}
      liftIO
      {-** from System.IO -}
      {-| Reexported from "System.IO" in __base__: -}
    , Handle
    , stdin, stdout, stderr
    , hFlush
    , unsafePerformIO
      {-* Exception handling -}
      {-** from Control.Exception.Safe -}
      {-| Rexported from "Control.Exception.Safe" in the __safe-exceptions__ package: -}
    , Exception(..)
    , throw
    , bracket
    , catch
    ) where

import Control.Exception.Safe (Exception, throw, bracket, catch)
import Control.Monad.IO.Class (liftIO)
import System.IO (Handle, stdin, stdout, stderr, hFlush)
import System.IO.Unsafe (unsafePerformIO)

