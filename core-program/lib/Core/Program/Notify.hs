{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Helpers for watching files for changes and taking action in the event of a
change.
-}
module Core.Program.Notify
    ( {-* Notify -}
      waitForChange
    ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Data.Foldable (foldr, foldrM)
import System.FilePath.Posix (dropFileName)
import System.INotify (EventVariety(..), Event(..), withINotify
    , addWatch, removeWatch)

import Core.Data.Structures
import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Unlift

{-|
Watch for changes to a given list of files.

Ideally we'd just set up inotifies on these individual files, but that
doesn't work when programs like vim move the original file, save a new one,
then delete the renamed original. From previous work we know that
@CLOSE_WRITE@ is emitted reliably through these sequences, so we can just
check to see if a that happens on a filename we care about (rather then the
original inodes those files were stored in).

Before continuing we insert a 100ms pause to allow whatever the editor was
to finish its write and switcheroo sequence.
-}
waitForChange :: [FilePath] -> Program τ ()
waitForChange files =
  let
    f :: FilePath -> Set C.ByteString -> Set C.ByteString
    f path acc = insertElement (C.pack path) acc

    g :: FilePath -> Set C.ByteString -> Set C.ByteString
    g path acc = insertElement (C.pack (dropFileName path)) acc
  in do
    event "Watching for changes"

    let paths = foldr f emptySet files
    let dirs  = foldr g emptySet files

    withContext $ \runProgram -> do
        block <- newEmptyMVar
        withINotify $ \notify -> do
            -- setup inotifies
            watches <- foldrM (\dir acc -> do
                runProgram (debugS "watching" dir)
                watch <- addWatch notify [CloseWrite] dir (\trigger ->
                    case trigger of
                        Closed _ (Just file) _  -> do
                            let path = if dir == "./"
                                        then file
                                        else dir <> file
                            runProgram (debugS "changed" path)
                            if containsElement path paths
                                then do
                                    runProgram (debugS "trigger" path)
                                    putMVar block False
                                else
                                    return ()
                        _ -> return ())
                return (watch:acc)) [] dirs

            -- wait
            _ <- readMVar block

            -- cleanup
            mapM_ removeWatch watches

    sleep 0.1
