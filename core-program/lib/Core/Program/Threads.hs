{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    createScope,
    forkThread,
    forkThread_,
    waitThread,
    waitThread_,
    waitThread',
    waitThreads_,

    -- * Helper functions
    concurrentThreads,
    concurrentThreads_,
    raceThreads,
    raceThreads_,

    -- * Internals
    Thread,
    unThread,
    getScope,
) where

import Control.Concurrent.MVar (
    newMVar,
    readMVar,
 )
import Control.Concurrent.STM (atomically, orElse)
import Control.Exception.Safe qualified as Safe (catch, throw)
import Control.Monad (
    void,
 )
import Control.Monad.Reader.Class (MonadReader (ask))
import Core.Program.Context
import Core.Program.Logging
import Core.System.Base
import Core.Text.Rope
import Ki qualified as Ki (Scope, Thread, await, awaitAll, fork, scoped)

{- |
A thread for concurrent computation.

(this wraps __ki__'s 'Thread' which in turn wraps __base__'s
'Control.Concurrent.ThreadId')

@since 0.6.0
-}
newtype Thread α = Thread (Ki.Thread α)

unThread :: Thread α -> Ki.Thread α
unThread (Thread a) = a

getScope :: Program τ (Maybe Ki.Scope)
getScope = do
    context <- ask
    pure (currentScopeFrom context)

{- |
Create a scope to enclose any subsequently spawned threads.

If any of the child threads throws an exception it will be propegated to this
parent thread and re-thrown.

When a scope exits, any threads that were spawed within it that are still
running will be killed.

@since 0.6.0
-}
createScope :: Program τ α -> Program τ α
createScope program = do
    context <- ask
    liftIO $ do
        Ki.scoped $ \scope -> do
            let context' =
                    context
                        { currentScopeFrom = Just scope
                        }
            subProgram context' program

{- |
Fork a thread. The child thread will run in the same 'Context' as the calling
'Program', including sharing the user-defined application state value.

In order to use this you /must/ be within an enclosing scope created with
'createScope'.

Threads that are launched off as children are on their own! If the code in the
child thread throws an exception that is /not/ caught within that thread, the
exception will kill the thread. Threads dying without telling anyone is a bit
of an anti-pattern, so this library logs a warning-level log message if this
happens.

(this wraps __ki__\'s 'Ki.fork' which in turn wraps __base__'s
'Control.Concurrent.forkIO')

@since 0.2.7
-}
forkThread :: Program τ α -> Program τ (Thread α)
forkThread program = do
    context <- ask
    let i = startTimeFrom context
    let v = currentDatumFrom context
    let scope = case currentScopeFrom context of
            Nothing -> error "Attempt to fork a thread outside of an enclosing scope"
            Just scope' -> scope'

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

        a <- Ki.fork scope $ do
            Safe.catch
                ( do
                    subProgram context' program
                )
                ( \(e :: SomeException) -> do
                    let text = intoRope (displayException e)
                    subProgram context' $ do
                        internal "Uncaught exception ending thread"
                        internal ("e = " <> text)
                    Safe.throw e
                )

        return (Thread a)

{- |
Fork a thread with 'forkThread' but do not wait for a result. This is on the
assumption that the sub program will either be a side-effect and over quickly,
or long-running daemon thread (presumably containing a 'Control.Monad.forever'
loop in it), never returning.

@since 0.5.2
-}
forkThread_ :: Program τ α -> Program τ ()
forkThread_ = void . forkThread

{- |
Wait for the completion of a thread, returning the result. This is a blocking
operation.

If the thread you are waiting on throws an exception it will be rethrown by
'waitThread'.

If the current thread making this call is cancelled (as a result of being on
the losing side of 'concurrentThreads' or 'raceThreads' for example, or due to
an explicit call to 'cancelThread'), then the thread you are waiting on will
be cancelled. This is necessary to ensure that child threads are not leaked if
you nest `forkThread`s.

(this wraps __async__\'s 'Control.Concurrent.Async.wait', taking care to
ensure the behaviour described above)

@since 0.2.7
-}
waitThread :: Thread α -> Program τ α
waitThread (Thread a) = liftIO $ do
    atomically $ do
        Ki.await a

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
waitThread_ t = void (waitThread t)

{- |
Wait for a thread to complete, returning the result if the computation was
successful or the exception if one was thrown by the child thread.

This basically is convenience for calling `waitThread` and putting `catch`
around it, but as with all the other @wait*@ functions this ensures that if
the thread waiting is killed the cancellation is propagated to the thread
being watched as well.

(this wraps __async__\'s 'Control.Concurrent.Async.waitCatch')

@since 0.4.5
-}
waitThread' :: Thread α -> Program τ (Either SomeException α)
waitThread' (Thread a) = liftIO $ do
    Safe.catch
        ( do
            result <- atomically $ do
                Ki.await a
            pure (Right result)
        )
        ( \(e :: SomeException) -> do
            pure (Left e)
        )

{- |
Wait for all child threads in the current scope to complete.

This function is intended for the scenario where you fire off a number of
worker threads with `forkThread` but rather than leaving them to run
independantly, you need to wait for them all to complete.

If the thread calling 'waitThreads_' is killed, then all the threads being
waited upon will also be killed. This often occurs within a timeout or similar
control measure implemented using 'raceThreads_'. Should the thread that
spawned all the workers and is waiting for their results be told to cancel
because it lost the "race", the child threads need to be told in turn to
cancel so as to avoid those threads being leaked and continuing to run as
zombies. The machinery underlying this function takes care of that.

(this wraps __ki__\'s 'Ki.awaitAll' )

@since 0.6.0
-}
waitThreads_ :: Program τ ()
waitThreads_ = do
    context <- ask
    scope <- case currentScopeFrom context of
        Nothing -> error "Invalid use of waitThreads_ without an enclosing scope"
        Just value -> pure value

    liftIO $ do
        atomically $ do
            Ki.awaitAll scope

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

@since 0.4.0
-}
concurrentThreads :: Program τ α -> Program τ β -> Program τ (α, β)
concurrentThreads one two = do
    context <- ask
    liftIO $ do
        Ki.scoped $ \scope -> do
            a1 <- Ki.fork scope $ do
                subProgram context one

            a2 <- Ki.fork scope $ do
                subProgram context two

            atomically $ do
                result1 <- Ki.await a1
                result2 <- Ki.await a2

                pure (result1, result2)

{- |
Fork two threads and wait for both to finish.

This is the same as calling 'forkThread' and 'waitThread_' twice, except that
if either sub-program fails with an exception the other program which is still
running will be cancelled and the original exception is then re-thrown.

(this wraps __async__\'s 'Control.Concurrent.Async.concurrently_')

@since 0.4.0
-}
concurrentThreads_ :: Program τ α -> Program τ β -> Program τ ()
concurrentThreads_ one two = void (concurrentThreads one two)

{- |
Fork two threads and race them against each other. This blocks until one or
the other of the threads finishes. The return value will be 'Left' @α@ if the
first program (@one@) completes first, and 'Right' @β@ if it is the second
program (@two@) which finishes first. The sub program which is still running
will be cancelled with an exception.

@
    result <- 'raceThreads' one two
    case result of
        'Left' a -> do
            -- one finished first
        'Right' b -> do
            -- two finished first
@

For a variant that ingores the return value and just races the threads see
'raceThreads_' below.

(this wraps __async__\'s 'Control.Concurrent.Async.race')

@since 0.4.0
-}
raceThreads :: Program τ α -> Program τ β -> Program τ (Either α β)
raceThreads one two = do
    context <- ask
    liftIO $ do
        Ki.scoped $ \scope -> do
            t1 <- Ki.fork scope $ do
                result1 <- subProgram context{currentScopeFrom = Just scope} one
                pure (Left result1)

            t2 <- Ki.fork scope $ do
                result2 <- subProgram context{currentScopeFrom = Just scope} two
                pure (Right result2)

            result <- atomically $ do
                orElse
                    (Ki.await t1)
                    (Ki.await t2)

            pure result

{- |
Fork two threads and race them against each other. When one action completes
the other will be cancelled with an exception. This is useful for enforcing
timeouts:

@
    'raceThreads_'
        ('Core.Program.Execute.sleepThread' 300)
        (do
            -- We expect this to complete within 5 minutes.
            performAction
        )
@

(this wraps __async__\'s 'Control.Concurrent.Async.race_')

@since 0.4.0
-}
raceThreads_ :: Program τ α -> Program τ β -> Program τ ()
raceThreads_ one two = void (raceThreads one two)
