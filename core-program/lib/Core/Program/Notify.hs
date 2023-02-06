{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Helpers for watching files for changes and taking action in the event of a
change.
-}
module Core.Program.Notify
    ( waitForChange
    ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Core.Data.Structures
import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Unlift
import Data.Foldable (foldrM)
import System.Directory (canonicalizePath)
import System.FSNotify (Event (..), eventPath, watchDir, withManager)
import System.FilePath (dropFileName)

{- |
Watch for changes to a given list of files.

Before continuing we insert a 100ms pause to allow whatever the editor was to
finish its write and switcheroo sequence.
-}

--
-- Ideally we'd just set up inotifies on these individual files, but that
-- doesn't work when programs like vim move the original file, save a new one,
-- then delete the renamed original.
--
-- From previous work we know that @CLOSE_WRITE@ is emitted reliably by inotify
-- on Linux through these sequences. We need to continue testing to assure
-- ourselves that the __fsnotify__ package's @Modify@ represents this
-- accurately.
--
waitForChange :: [FilePath] -> Program τ ()
waitForChange files =
    let f :: FilePath -> Set FilePath -> Set FilePath
        f path acc = insertElement path acc

        g :: FilePath -> Set FilePath -> Set FilePath
        g path acc = insertElement (dropFileName path) acc
    in  do
            info "Watching for changes"

            canonical <- mapM (liftIO . canonicalizePath) files
            let paths = foldr f emptySet canonical
            let dirs = foldr g emptySet files

            withContext $ \runProgram -> do
                block <- newEmptyMVar
                withManager $ \manager -> do
                    -- setup watches
                    stoppers <-
                        foldrM
                            ( \dir acc -> do
                                runProgram (debugS "watching" dir)
                                stopper <-
                                    watchDir
                                        manager
                                        dir
                                        ( \trigger -> case trigger of
                                            Modified file _ _ -> do
                                                if containsElement file paths
                                                    then True
                                                    else False
                                            _ -> False
                                        )
                                        ( \trigger -> do
                                            runProgram (debugS "trigger" (eventPath trigger))
                                            putMVar block False
                                        )
                                return (stopper : acc)
                            )
                            []
                            dirs

                    -- wait
                    _ <- readMVar block

                    sequence_ stoppers
                    return ()

            sleepThread 0.1
