{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Utility functions for running 'Program' actions concurrently.

Haskell uses green threads: small lines of work that are scheduled down onto
actual execution contexts (set by default by this library to be one per core).
Haskell threads are incredibly lightweight, and you are encouraged to use them
freely. Haskell provides a rich ecosystem of tools to do work concurrently and
to communicate safely between threads.

This module provides wrappers around some of these primatives so you can use
them easily from the 'Program' monad.

Note that when you fire off a new thread the top-level application state is
/shared/; it's the same @τ@ inherited from the parent 'Program'.
-}
module Core.Program.Threads (
    -- * Concurrency
    forkThread,
    waitThread,
    waitThread_,
    linkThread,

    -- * Helper functions
    concurrentThreads,
    concurrentThreads_,
    raceThreads,
    raceThreads_,

    -- * Internals
    Thread,
    unThread,
) where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async (
    async,
    concurrently,
    concurrently_,
    link,
    race,
    race_,
    wait,
 )
import Control.Concurrent.MVar (
    newMVar,
    readMVar,
 )
import qualified Control.Exception.Safe as Safe (catch)
import Control.Monad (
    void,
 )
import Control.Monad.Reader.Class (MonadReader (ask))
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Text.Rope

{- |
A thread for concurrent computation.

(this wraps __async__'s 'Async')
-}
newtype Thread α = Thread (Async α)

unThread :: Thread α -> Async α
unThread (Thread a) = a

{- |
Fork a thread. The child thread will run in the same @Context@ as the calling
@Program@, including sharing the user-defined application state value.

(this wraps __async__\'s 'Control.Concurrent.Async.async' which in turn wraps
__base__'s 'Control.Concurrent.forkIO')

@since 0.2.7
-}
forkThread :: Program τ α -> Program τ (Thread α)
forkThread program = do
    context <- ask
    let i = startTimeFrom context
    let v = currentDatumFrom context

    liftIO $ do
        -- if someone calls resetTimer in the thread it should just be that
        -- thread's local duration that is affected, not the parent. We simply
        -- make a new MVar and copy the current start time into it.

        start <- readMVar i
        i' <- newMVar start

        -- we also need to fork the current Datum, in the same way that we do
        -- when we create a nested span. We do this simply by creating a new
        -- MVar so that when the new thread updates the attached metadata
        -- it'll be evolving a different object.

        datum <- readMVar v
        v' <- newMVar datum

        let context' =
                context
                    { startTimeFrom = i'
                    , currentDatumFrom = v'
                    }

        -- fork, and run nested program

        a <- Async.async $ do
            Safe.catch
                (subProgram context' program)
                ( \(e :: SomeException) ->
                    let text = intoRope (displayException e)
                     in do
                            subProgram context' $ do
                                warn "Uncaught exception in thread"
                                debug "e" text
                            throw e
                )

        return (Thread a)

{- |
Wait for the completion of a thread, returning the result. This is a blocking
operation.

(this wraps __async__\'s 'wait')

@since 0.2.7
-}
waitThread :: Thread α -> Program τ α
waitThread (Thread a) = liftIO $ Async.wait a

{- |
Wait for the completion of a thread, discarding its result. This is
particularly useful at the end of a do-block if you're waiting on a worker
thread to finish but don't need its return value, if any; otherwise you have
to explicily deal with the unused return value:

@
    _ <- 'waitThread' t1
    'return' ()
@

which is a bit tedious. Instead, you can just use this convenience function:

@
    'waitThread_' t1
@

The trailing underscore in the name of this function follows the same
convetion as found in "Control.Monad", which has 'Control.Monad.mapM_' which
does the same as 'Control.Monad.mapM' but which likewise discards the return
value.

@since 0.2.7
-}
waitThread_ :: Thread α -> Program τ ()
waitThread_ = void . waitThread

{- |
Ordinarily if an exception is thrown in a forked thread that exception is
silently swollowed (well, almost silently; we log a warning-level message if
an uncaught exception kills a thread). If you instead need the exception to
propegate back to the parent thread, you can link the two together using this
function.

@since 0.4.2
-}
linkThread :: Thread α -> Program τ ()
linkThread (Thread a) = do
    liftIO $ do
        Async.link a

{- |
Fork two threads and wait for both to finish. The return value is the pair of
each action's return types.

This is the same as calling 'forkThread' and 'waitThread' twice, except that
if either sub-program fails with an exception the other program which is still
running will be cancelled and the original exception is then re-thrown.

@
    (a,b) <- 'concurrentThreads' one two

    -- continue, doing something with both results.
@

For a variant that ingores the return values and just waits for both see
'concurrentThreads_' below.

(this wraps __async__\'s 'Control.Concurrent.Async.concurrently')

@since 4.0.0
-}
concurrentThreads :: Program τ α -> Program τ β -> Program τ (α, β)
concurrentThreads one two = do
    context <- ask
    liftIO $ do
        Async.concurrently
            (subProgram context one)
            (subProgram context two)

{- |
Fork two threads and wait for both to finish.

This is the same as calling 'forkThread' and 'waitThread_' twice, except that
if either sub-program fails with an exception the other program which is still
running will be cancelled and the original exception is then re-thrown.

(this wraps __async__\'s 'Control.Concurrent.Async.concurrently_')

@since 4.0.0
-}
concurrentThreads_ :: Program τ α -> Program τ β -> Program τ ()
concurrentThreads_ one two = do
    context <- ask
    liftIO $ do
        Async.concurrently_
            (subProgram context one)
            (subProgram context two)

{- |
Fork two threads and race them against each other. This blocks until one or
the other of the threads finishes. The return value will be 'Left' @α@ if the
first program (@one@) completes first, and 'Right' @β@ if it is the second
program (@two@) which finishes first. The sub program which is still running
will be cancelled with an exception.

@
    result <- 'raceThreads' one two
    case result of
        Left a -> do
            -- one finished first
        Right b -> do
            -- two finished first
@

For a variant that ingores the return value and just races the threads see
'raceThreads_' below.

(this wraps __async__\'s 'Control.Concurrent.Async.race')

@since 4.0.0
-}
raceThreads :: Program τ α -> Program τ β -> Program τ (Either α β)
raceThreads one two = do
    context <- ask
    liftIO $ do
        Async.race
            (subProgram context one)
            (subProgram context two)

{- |
Fork two threads and race them against each other. When one action completes
the other will be cancelled with an exception. This is useful for enforcing
timeouts:

@
    'raceThreads_'
        ('sleepThread' 300)
        (do
            -- We expect this to complete within 5 minutes.
            performAction
        )
@

(this wraps __async__\'s 'Control.Concurrent.Async.race_')

@since 4.0.0
-}
raceThreads_ :: Program τ α -> Program τ β -> Program τ ()
raceThreads_ one two = do
    context <- ask
    liftIO $ do
        Async.race_
            (subProgram context one)
            (subProgram context two)
