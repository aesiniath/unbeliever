{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Re-exports of Haskell base and GHC system libraries.
-}
module Core.System.Base (
    -- * Input/Output

    -- ** from Control.Monad.IO.Class

    -- | Re-exported from "Control.Monad.IO.Class" in __base__:
    liftIO,
    MonadIO,

    -- ** from System.IO

    -- | Re-exported from "System.IO" in __base__:
    Handle,
    IOMode (..),
    withFile,
    stdin,
    stdout,
    stderr,
    hFlush,
    unsafePerformIO,

    -- * Exception handling

    -- ** from Control.Exception.Safe

    -- | Re-exported from "Control.Exception.Safe" in the __safe-exceptions__ package:
    Exception (..),
    SomeException,
    impureThrow,
    bracket,
    finally,
) where

import Control.Exception (
    Exception (..),
    SomeException,
 )
import Control.Exception.Safe (
    bracket,
    finally,
    impureThrow,
 )
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (..), hFlush, stderr, stdin, stdout, withFile)
import System.IO.Unsafe (unsafePerformIO)
