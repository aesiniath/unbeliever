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
    , unsafePerformIO
    ) where

import Control.Monad.IO.Class (liftIO)
import System.IO (Handle, stdin, stdout, stderr)
import System.IO.Unsafe (unsafePerformIO)

