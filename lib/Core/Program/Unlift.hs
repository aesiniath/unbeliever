{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
The 'Program' monad is an instance of 'MonadIO', which makes sense; it's
just a wrapper around doing 'IO' and you call it using 'execute' from the
top-level @main@ action that is the entrypoint to any program.  So when you
need to actually do some I/O or interact with other major libraries in the
Haskell ecosystem, you need to get back to 'IO' and you use 'liftIO' to do
it:

@
main :: 'IO' ()
main = 'execute' $ do
    -- now in the Program monad
    'write' "Hello there"

    'liftIO' $ do
        -- do something in IO
        source <- readFile "hello.c"
        compileSourceCode source

    -- back in Program monad
    'write' \"Finished\"
@

and this is a perfectly reasonably pattern.

Sometimes, however, you want to somehow get back to the 'Program' monad
from there, and that's tricky; you can't just 'execute' a new program (and
don't try: we've already initialized output and logging channels, signal
handlers, your application context, etc).

@
main :: 'IO' ()
main = 'execute' $ do
    -- now in the Program monad
    'write' "Hello there"

    'liftIO' $ do
        -- do something in IO
        source <- readFile "hello.c"
        -- log that we're starting compile      ... how???
        result <- compileSourceCode source
        case result of
            Right object -> linkObjectCode object
            Left err     -> -- debug the error  ... how???

    -- back in Program monad
    'write' \"Finished\"
@

We have a problem, because we'd like to do is use, say, 'debug' to log the
compiler error, but we have no way to unlift back out of 'IO' to get to the
'Program' monad.

To workaround this, we offer 'withContext'. It gives you a function that
you can use within your lifted 'IO' to run a 'Program' action:

@
main :: 'IO' ()
main = 'execute' $ do
    -- now in the Program monad
    'write' "Hello there"

    'withContext' $ \\runProgram -> do
        -- do something in IO
        source <- readFile "hello.c"

        runProgram $ do
            -- now in Program monad
            'event' \"Starting compile...\"
            'event' \"Nah. Changed our minds\"
            'event' \"Ok, fine, compile the thing\"

        -- more IO
        result <- compileSourceCode source
        case result of
            'Right' object -> linkObjectCode object
            'Left' err     -> runProgram ('debugS' err)

    -- back in Program monad
    'write' \"Finished\"
@

Sometimes Haskell type inference can give you trouble because it tends to
assume you mean what you say with the last statement of do-notation block.
If you've got the type wrong you'll get an error, but in an odd place,
probably at the top. This can be confusing. If you're having trouble with
the types try putting @return ()@ at the end of your subprogram.
-}
module Core.Program.Unlift
    (
        {-* Useful actions -}
        withContext
        {-* Internals -}
      , getContext
      , subProgram
    ) where

import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

import Core.Program.Context
import Core.System.Base

{-|
Get the internal @Context@ of the running @Program@.
-}
getContext :: Program τ (Context τ)
getContext = do
    context <- ask
    return context

{-|
Run a subprogram from within a lifted 'IO' block.
-}
subProgram :: Context τ -> Program τ α -> IO α
subProgram context (Program reader) = do
    runReaderT reader context

{-|
This gives you a function that you can use within your lifted 'IO' actions
to return to the 'Program' monad.

The type signature of this function is a bit involved, but this example
shows that the lambda gives you a /function/ as its argument (we recommend
you name it @__runProgram__@ for consistency) which gives you a way to run a
subprogram, be that a single action like writing to terminal or logging, or
a larger action in a do-notation block:

@
main :: IO ()
main = execute $ do
    'withContext' $ \\runProgram -> do
        -- in IO monad, lifted
        -- (just as if you had used liftIO)

        ...

        __runProgram__ $ do
            -- now unlifted, back to Program monad

        ...
@

Think of this as 'liftIO' with an escape hatch.

This function is named 'withContext' because it is a convenience around the
following pattern:

@
    context <- 'getContext'
    liftIO $ do
        ...
        'subProgram' context $ do
            -- now in Program monad
        ...
@
-}
-- I think I just discovered the same pattern as **unliftio**? Certainly
-- the signature is similar. I'm not sure if there is any benefit to
-- restating this as a `withRunInIO` action; we're deliberately trying to
-- constrain the types.
withContext
    :: ((forall β. Program τ β -> IO β) -> IO α)
    -> Program τ α
withContext action = do
    context <- getContext
    let runThing = subProgram context
    liftIO (action runThing)

