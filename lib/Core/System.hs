{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

--
-- | Re-exports of Haskell base and GHC system libraries
--
module Core.System
    ( liftIO
    , Handle
    , stdin, stdout, stderr
    , hFlush
    , unsafePerformIO
    , Exception(..)
    , throw
    , bracket
    , catch
    ) where

import Control.Exception.Safe (Exception, throw, bracket, catch)
import Control.Monad.IO.Class (liftIO)
import System.IO (Handle, stdin, stdout, stderr, hFlush)
import System.IO.Unsafe (unsafePerformIO)

