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
Utility functions for building programs which consume work off of a queue.

Frequently you need to receive items from an external system and perform work
on them. One way to structure such a program is to feed the items into a queue
and then consume those items one at a time. That, of course, is
slow—especially when then worker has to itself carry out computationally
intensive tasks or interact itself with external systems. So we want to have
multiple workers running, but only to an extent limited by the number of cores
available, the number of external connections allowed, or some other
constraint.

This library allows you to add items to a queue, then launch worker threads to
consume those items at up to a specified maximum amount of concurrency.
-}
module Core.Program.Workers
    ( -- * Work Queue
      newQueue
    , writeQueue
    , writeQueue'
    , finishQueue

      -- * Worker Threads
    , runWorkers_
    , mapWorkers

      -- * Internals
    , Queue
    , unQueue
    , getMachineSize
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, readTQueue, unGetTQueue, writeTQueue)
import Control.Monad
    ( forM
    )
import Core.Program.Context
import Core.Program.Threads
import Core.System.Base
import GHC.Conc (getNumCapabilities)

{- |
Report back the number of processor cores that are available as Haskell
"capabilities" (this was set when you launched the program with
'Core.Program.Execute.execute'). This can best be used to set the number of
concurrent worker threads when running 'runWorkers_' or 'mapWorkers'.

@since 0.6.9
-}
getMachineSize :: Program τ Int
getMachineSize = liftIO $ do
    getNumCapabilities

{- |
A queue which has an end, someday.

(this is a thin wrapper over the __stm__ 'TQueue' type)

@since 0.6.9
-}
newtype Queue α = Queue (TQueue (Maybe α))

{- |
Initialize a new queue.

@since 0.6.9
-}
newQueue :: Program τ (Queue α)
newQueue = do
    queue <- liftIO $ do
        newTQueueIO
    pure (Queue queue)

{- |
Add an item to the queue.

@since 0.6.9
-}
writeQueue :: Queue α -> α -> Program τ ()
writeQueue (Queue queue) item = do
    liftIO $ do
        atomically $ do
            writeTQueue queue (Just item)

{- |
Add a list of items to the queue.

@since 0.6.9
-}
writeQueue' :: Foldable ω => Queue α -> ω α -> Program τ ()
writeQueue' (Queue queue) items = do
    liftIO $ do
        atomically $ do
            mapM_
                ( \item ->
                    writeTQueue queue (Just item)
                )
                items

{- |
Indicate that you are finished adding queue, thereby allowing the worker
threads consuming from the queue to complete and return.

Remember that you can call at any time, even before you have launched the
worker threads with 'runWorkers_'.

@since 0.6.9
-}
finishQueue :: Queue α -> Program τ ()
finishQueue (Queue queue) = do
    liftIO $ do
        atomically $ do
            writeTQueue queue Nothing

{- |
Access the underlying queue. We make use of the STM 'TQueue' type, so you'll
want the following imports:

@
import "Control.Concurrent.STM" ('atomically')
import "Control.Concurrent.STM.TQueue" ('TQueue', 'writeTQueue')
@

Having accessed the underlying queue you can write items, wrapped in 'Just', to
it directly:

@
    'liftIO' $ do
        'atomically' $ do
            'writeTQueue' queue ('Just' item)
@

A 'Nothing' written to the underlying queue will signal the worker threads
that the end of input has been reached and they can safely return.

@since 0.6.9
-}
unQueue :: Queue α -> TQueue (Maybe α)
unQueue (Queue queue) = queue

{- |
Run a pool of worker threads which consume items off the work queue.

Once you have an action that enqueues items with 'writeQueue' you can then
launch the worker threads:

@
    'runWorkers_' 16 worker queue
@

consuming 16 items at a time concurrently in this example.

It is assumed that the workers have a way of communicating their results
onwards, either because they are side-effecting in the real world themselves,
or because you have passed in some  'Control.Concurrent.MVar' or 'TQueue' to
collect the results.

@since 0.6.9
-}
runWorkers_ :: Int -> (α -> Program τ ()) -> Queue α -> Program τ ()
runWorkers_ n action (Queue queue) = do
    createScope $ do
        ts <- forM [1 .. n] $ \_ -> do
            forkThread $ do
                loop
        _ <- waitThreads' ts
        pure ()
  where
    loop = do
        possibleItem <- liftIO $ do
            atomically $ do
                readTQueue queue -- blocks
        case possibleItem of
            Nothing -> do
                --
                -- We put the Nothing back so that other workers can also shutdown.
                --
                liftIO $ do
                    atomically $ do
                        unGetTQueue queue Nothing
            Just item -> do
                --
                -- Do the work
                --
                action item
                loop

{- |
Map a pool of workers over a list concurrently.

Simply forking one Haskell thread for every item in a list is a suprisingly
reasonable choice in many circumstances given how good Haskell's concurrency
machinery is, and in this library can be achieved by 'Control.Monad.forM'ing
'forkThread' over a list of items. But if you need tighter control over the
amount of concurrency—as is often the case when doing something
computationally heavy or making requests of an external service with known
limitations—then you are better off using this convenience function.

(this was originally modelled on __async__\'s
'Control.Concurrent.Async.mapConcurrently'. That implementation has the
drawback that the number of threads created is set by the size of the
structure being traversed. Here we set the amount of concurrency explicitly.)

Be aware that the order of items in the output list is non-deterministic and
will depend on the order that the action function completes, not the order of
items in the input.

@since 0.6.9
-}
mapWorkers :: Int -> (α -> Program τ β) -> [α] -> Program τ [β]
mapWorkers n action items = do
    inputs <- newQueue

    outputs <- liftIO $ do
        newTQueueIO :: IO (TQueue β)

    --
    -- Load the input list into a queue followed by a terminator.
    --

    writeQueue' inputs items
    finishQueue inputs

    --
    -- Invoke the general concurrent workers tool above to process the queue.
    --

    runWorkers_
        n
        ( \item -> do
            result <- action item
            liftIO $ do
                atomically $ do
                    writeTQueue outputs result
        )
        inputs

    --
    -- Convert the results back to a list.
    --

    liftIO $ do
        atomically $ do
            flushTQueue outputs
