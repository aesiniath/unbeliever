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
module Core.Program.Threads
    ( -- * Concurrency
      createScope
    , forkThread
    , forkThread_
    , linkThread
    , waitThread
    , waitThread_
    , waitThread'
    , waitThreads'
    , cancelThread

      -- * Helper functions
    , concurrentThreads
    , concurrentThreads_
    , raceThreads
    , raceThreads_
    , timeoutThread

      -- * Internals
    , Thread
    , unThread
    , Terminator (..)
    , Timeout (..)
    ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVarIO)
import Control.Exception.Safe qualified as Safe (catch, finally, onException, throw)
import Control.Monad
    ( forM
    , forM_
    , void
    )
import Control.Monad.Reader.Class (MonadReader (ask))
import Core.Data.Structures
import Core.Program.Context
import Core.Program.Exceptions
import Core.Program.Execute
import Core.Program.Logging
import Core.System.Base
import Core.Text.Rope

{- |
A thread for concurrent computation.

(this wraps __base__'s 'Control.Concurrent.ThreadId' along with a holder for
the result of the thread)

@since 0.6.0
-}
data Thread α = Thread
    { threadPointerOf :: ThreadId
    , threadOutcomeOf :: MVar (Either SomeException α)
    }

unThread :: Thread α -> ThreadId
unThread = threadPointerOf

{- |
Create a scope to enclose any subsequently spawned threads as a single group.
Ordinarily threads launched in Haskell are completely indepedent. Creating a
scope allows you to operate on a set of threads as a single group with
bi-directional exception passing. This is the basis of an approach called
/structured concurrency/.

When the execution flow exits the scope, any threads that were spawned within
it that are still running will be killed.

If any of the child threads within the scope throws an exception, the other
remaining threads will be killed and then the original exception will be
propegated to this parent thread and re-thrown.

@since 0.6.0
-}
createScope :: Program τ α -> Program τ α
createScope program = do
    context <- ask

    liftIO $ do
        scope <- newTVarIO emptySet

        let context' =
                context
                    { currentScopeFrom = scope
                    }

        Safe.finally
            ( do
                subProgram context' program
            )
            ( do
                pointers <- readTVarIO scope
                forM_ pointers killThread
            )

{- |
Fork a thread. The child thread will run in the same 'Context' as the calling
'Program', including sharing the user-defined application state value.

If you want to find out what the result of a thread was use 'waitThread' on
the 'Thread' object returned from this function. For example:

@
    t1 <- 'forkThread' $ do
        'Core.Program.Logging.info' \"Doing interesting stuff concurrently\"
        'pure' True

    ...

    result <- 'waitThread' t1

    if result
        then -- expected
        else -- not good
@

If you don't need the result, you can use 'forkThread_' instead.

Threads that are launched off as children are on their own! If the code in the
child thread throws an exception that is /not/ caught within that thread, the
exception will kill the thread. Threads dying without telling anyone is a bit
of an anti-pattern, so this library logs a warning-level log message if this
happens.

(this function wraps __base__'s 'Control.Concurrent.forkIO')

/Concerning telemetry/

Note that threads inherit the telemetry state from their parent. If you are
using the tracing features from __core-telemetry__ any telemetry
registered in that side task will be included in the enclosing span active in the thread
that spawned the thread:

@
    t2 <- 'forkThread' $ do
        'Core.Program.Logging.info' \"Performing quick side task\"
        'Core.Telemetry.Observability.telemetry'
            [ ''Core.Telemetry.Observability.metric' \"counter\" 42
            ]
        ...

@

In this case the @\"counter\"@ field in the parent span will get the value
@42@. This is appropriate for the common case where you are doing small side
tasks concurrently to accelerate a larger computation.

But at other times you are launching off a fully independent control flow and
want it to have its own telemetry. In those cases, you'll want to start a new
span (or even a new trace) immediately after forking the thread:

@
    'forkThread_' $ do
        'Core.Telemetry.Observability.encloseSpan' \"subTask\" $ do
            ...
@

any telemetry from this worker thread will be appropriately nested in a new
child span called @\"subTask\"@.

@since 0.2.7
-}
forkThread :: Program τ α -> Program τ (Thread α)
forkThread program = do
    context <- ask
    let i = startTimeFrom context
    let scope = currentScopeFrom context

    liftIO $ do
        -- if someone calls resetTimer in the thread it should just be that
        -- thread's local duration that is affected, not the parent. We simply
        -- make a new MVar and copy the current start time into it.

        start <- readMVar i
        i' <- newMVar start

        let context' =
                context
                    { startTimeFrom = i'
                    }

        -- fork, and run nested program

        outcome <- newEmptyMVar

        pointer <- forkIO $ do
            Safe.catch
                ( do
                    actual <- subProgram context' program
                    putMVar outcome (Right actual)
                )
                ( \(e :: SomeException) -> do
                    let text = intoRope (displayException e)
                    subProgram context' $ do
                        internal "Uncaught exception ending thread"
                        internal ("e = " <> text)
                    putMVar outcome (Left e)
                )

        atomically $ do
            modifyTVar' scope (\pointers -> insertElement pointer pointers)

        return
            ( Thread
                { threadPointerOf = pointer
                , threadOutcomeOf = outcome
                }
            )

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
the current scope exiting), then the thread you are waiting on will be
cancelled too. This is necessary to ensure that child threads are not leaked
if you nest `forkThread`s.

@since 0.2.7
-}
waitThread :: Thread α -> Program τ α
waitThread thread = do
    result <- waitThread' thread

    case result of
        Left problem -> Safe.throw problem
        Right actual -> pure actual

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
waitThread_ thread = void (waitThread thread)

{- |
Wait for a thread to complete, returning the result if the computation was
successful or the exception if one was thrown by the child thread.

This basically is convenience for calling `waitThread` and putting `catch`
around it, but as with all the other @wait*@ functions this ensures that if
the thread waiting is killed the cancellation is propagated to the thread
being watched as well.

@since 0.4.5
-}
waitThread' :: Thread α -> Program τ (Either SomeException α)
waitThread' thread = do
    context <- ask
    let scope = currentScopeFrom context
    let outcome = threadOutcomeOf thread
    let pointer = threadPointerOf thread

    liftIO $ do
        Safe.onException
            ( do
                result <- readMVar outcome -- blocks!
                atomically $ do
                    modifyTVar' scope (\pointers -> removeElement pointer pointers)
                pure result
            )
            ( do
                killThread pointer
                atomically $ do
                    modifyTVar' scope (\pointers -> removeElement pointer pointers)
            )

{- |
Wait for many threads to complete. This function is intended for the scenario
where you fire off a number of worker threads with `forkThread` but rather
than leaving them to run independantly, you need to wait for them all to
complete.

The results of the threads that complete successfully will be returned as
'Right' values. Should any of the threads being waited upon throw an
exception, those exceptions will be returned as 'Left' values.

If you don't need to analyse the failures individually, then you can just
collect the successes using "Data.Either"'s 'Data.Either.rights':

@
    responses <- 'waitThreads''

    'info' "Aggregating results..."
    combineResults ('Data.Either.rights' responses)
@

Likewise, if you /do/ want to do something with all the failures, you might
find 'Data.Either.lefts' useful:

@
    'mapM_' ('warn' . 'intoRope' . 'displayException') ('Data.Either.lefts' responses)
@

If the thread calling 'waitThreads'' is cancelled, then all the threads being
waited upon will also be cancelled. This often occurs within a timeout or
similar control measure implemented using 'raceThreads_'. Should the thread
that spawned all the workers and is waiting for their results be told to
cancel because it lost the "race", the child threads need to be told in turn
to cancel so as to avoid those threads being leaked and continuing to run as
zombies. This function takes care of that.

(this extends 'waitThread'' to work across a list of Threads, taking care to
ensure the cancellation behaviour described throughout this module)

@since 0.4.5
-}
waitThreads' :: [Thread α] -> Program τ [Either SomeException α]
waitThreads' threads = do
    context <- ask
    liftIO $ do
        Safe.onException
            ( do
                subProgram context $ do
                    forM threads waitThread'
            )
            ( do
                --
                -- This is here because if this thread is cancelled it will
                -- only be _one_ of the waitThread above that receives the
                -- exception. All the other child threads need to be killed
                -- too.
                --

                let scope = currentScopeFrom context

                forM_ threads $ \thread -> do
                    let pointer = threadPointerOf thread
                    killThread pointer

                    atomically $ do
                        modifyTVar' scope (\pointers -> removeElement pointer pointers)
            )

{- |
Cancel a thread.

Be careful when using this. If you are planning cancel a worker thread then
the main thread that is 'waitThread'ing on it will /throw an exception/,
specifically 'ThreadCancelled' (unless something else has already thrown an
exception in which case /that/ will be thrown instead). In this scenario you
will need to 'Core.Program.Exceptions.catch' around your waiting function
otherwise the uncaught exception will continue to unwind your execution stack
and probably end your program.

(this wraps __base__\'s 'Control.Concurrent.killThread'. The underlying
mechanism used is to throw the 'GHC.Conc.ThreadKilled' exception to the other
thread. That exception is asynchronous, so will not be trapped by a
'Core.Program.Exceptions.catch' block and will indeed cause the thread
receiving the exception to come to an end)

@since 0.4.5
-}
cancelThread :: Thread α -> Program τ ()
cancelThread thread = do
    liftIO $ do
        --
        -- There are some curiosities about what happens here. Someone
        -- waitThread'ing on a Thread is blocked on reading the outcome MVar.
        -- so to break that wait we put the Left value in. If the thread was
        -- already dead this has no effect, but if not, then this will
        -- initiate it rapidly being killing off.
        --
        let outcome = threadOutcomeOf thread
        result <- tryPutMVar outcome (Left (toException ThreadCancelled))
        case result of
            False -> do
                pure ()
            True -> do
                killThread (threadPointerOf thread)

{- |
When a thread is aborted with 'cancelThread' this value is used to mark a
failed computation inside the 'Thread'. Although it is not the mechanism used
internally to kill the computation, it /is/ the exception that is subsequently
rethrown from 'waitThread' if you are waiting on that thread to finish,
allowing you to 'Core.Program.Exceptions.catch' the case of a thread being
cancelled if necessary.

This is mostly here to differentiate from 'Control.Exception.ThreadKilled',
giving you some knowledge as to whether it was your explicit 'cancelThread'
that ended the thread, or something else. You need to handle it either way,
but sometimes you want to know the difference.

@since 0.6.8
-}
data Terminator = ThreadCancelled
    deriving (Show)

instance Exception Terminator

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

@since 0.4.0
-}
concurrentThreads :: Program τ α -> Program τ β -> Program τ (α, β)
concurrentThreads one two = do
    createScope $ do
        a1 <- forkThread one
        a2 <- forkThread two
        result1 <- waitThread a1
        result2 <- waitThread a2
        pure (result1, result2)

{- |
Fork two threads and wait for both to finish.

This is the same as calling 'forkThread' and 'waitThread_' twice, except that
if either sub-program fails with an exception the other program which is still
running will be cancelled and the original exception is then re-thrown.

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

@since 0.4.0
-}
raceThreads :: Program τ α -> Program τ β -> Program τ (Either α β)
raceThreads one two = do
    createScope $ do
        outcome <- liftIO $ do
            newEmptyMVar

        t1 <- forkThread $ do
            finally
                ( do
                    one
                )
                ( do
                    liftIO $ do
                        putMVar outcome (Left ())
                )

        t2 <- forkThread $ do
            finally
                ( do
                    two
                )
                ( do
                    liftIO $ do
                        putMVar outcome (Right ())
                )

        result <- liftIO $ do
            readMVar outcome

        case result of
            Left _ -> do
                result1 <- waitThread t1
                pure (Left result1)
            Right _ -> do
                result2 <- waitThread t2
                pure (Right result2)

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

@since 0.4.0
-}
raceThreads_ :: Program τ α -> Program τ β -> Program τ ()
raceThreads_ one two = void (raceThreads one two)

linkThread :: Thread α -> Program τ ()
linkThread _ = pure ()
{-# DEPRECATED linkThread "Exceptions are bidirectional so linkThread no longer needed" #-}

{- |
If a timeout is exceeded this exception will be thrown by 'timeoutThread'.

@since 0.6.9
-}
data Timeout = Timeout deriving (Show)

instance Exception Timeout

{- |
Run a program that needs to complete before the given number of seconds have
elapsed. This will return the result of the sub program or throw the 'Timeout'
exception if the limit is exceeded.

@since 0.6.9
-}
timeoutThread :: Rational -> Program τ α -> Program τ α
timeoutThread seconds program = do
    result <-
        raceThreads
            ( do
                sleepThread seconds
                pure Timeout
            )
            ( do
                program
            )
    case result of
        Left e -> throw e
        Right a -> pure a
