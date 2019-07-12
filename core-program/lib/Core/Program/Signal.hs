{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Core.Program.Signal
(
    setupSignalHandlers
)
where

import Control.Concurrent.MVar (MVar, putMVar, modifyMVar_)
import Foreign.C.Types (CInt)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, hFlush, stdout)
import System.Posix.Signals (Handler(Catch), installHandler,
    sigINT, sigTERM, sigUSR1)

import Core.Program.Context

--
-- | Make a non-zero exit code which is 0b1000000 + the number of the
-- signal. Probably never need this (especaially given our attempt to
-- write out a human readable name for the signal caught) but it's a
-- convention we're happy to observe.
--
code :: CInt -> ExitCode
code signal = ExitFailure (128 + fromIntegral signal)

{-
    Technique to have a blocking MVar and signal handlers to set it
    adapted from code in vaultaire-common package's Vaultaire.Program,
    BSD3 licenced.
-}

interruptHandler :: MVar ExitCode -> Handler
interruptHandler quit = Catch $ do
    hPutStrLn stdout "\nInterrupt"
    hFlush stdout
    putMVar quit (code sigINT)

terminateHandler :: MVar ExitCode -> Handler
terminateHandler quit = Catch $ do
    hPutStrLn stdout "Terminating"
    hFlush stdout
    putMVar quit (code sigTERM)

logLevelHandler :: MVar Verbosity -> Handler
logLevelHandler v = Catch $ do
    hPutStrLn stdout "Signal"
    hFlush stdout
    modifyMVar_ v (\level -> case level of
            Output -> pure Debug
            Event  -> pure Debug
            Debug  -> pure Output)

--
-- | Install signal handlers for SIGINT and SIGTERM that set the exit
-- semaphore so that a Program's [minimal] cleanup can occur.
--
setupSignalHandlers :: MVar ExitCode -> MVar Verbosity -> IO ()
setupSignalHandlers quit level = do
    installHandler sigINT (interruptHandler quit) Nothing
    installHandler sigTERM (terminateHandler quit) Nothing
    installHandler sigUSR1 (logLevelHandler level) Nothing
    return ()
