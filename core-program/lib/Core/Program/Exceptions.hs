{-# LANGUAGE ImportQualifiedPost #-}

{- |
Facilities for handling exceptions.

The Haskell language itself doesn't treat exceptions specially, but the
Haskell /runtime/ does. Any I/O action can result in exceptions being thrown
and frequently do. Developers can define exceptions too, and use them to
signal anomolies.

In order to catch an exception you need to know the /type/ of that exception.
The way this is typically done is with the `ScopedTypeVariables` extension
turned on and then adding a type annotation around the @e@ variable in the
lambda passed to 'catch'.

@
    'catch'
        (do
            performSong \"This is my party and I\'ll cry if I want to\"
        )
        (\\(e :: FirstWorldProblem) -> do
            'Core.Program.Logging.critical' \"Someone is crying\"
            'Core.Program.Logging.debug' "e" ('Control.Exception.displayException' e)
            'Core.Program.Execute.terminate' 1
        )
@

which would work on the assumption that somewhere you have defined:

@
data FirstWorldProblem
    = PersonCrying
    | MyToastIsBurnt
    | SomeoneWrongOnInternet 'Core.Text.Rope'
    deriving 'Show'

instance 'Control.Exception.Exception' FirstWorldProblem
@

and that the @performSong@ function at some point does something like:

@
performSong :: Lyrics -> 'Program' 'None' ()
performSong lyrics = do
    ...
    'throw' PersonCrying
@

Keep in mind that exceptions are really for signalling failure and aren't
generally that recoverable. Their utility is that they unwind the call stack
from the point that failure occurs and get you back to somewhere you can
handle it, but in Haskell \"handling it\" really just means that you log the
problem and either go on to processing the next request or then outright
terminate the program.

Thus a good pattern for using exceptions effectively is to use small blocks of
pure code which can fail in the type 'Either' 'Core.Text.Rope' @a@, then
pattern matching on what you get back: if you get a 'Left' back then you
'throw' an exception, otherwise you return the value with 'pure' and continue:

@
    result <- case calculateInterestingThing inputs of
        'Left' problem -> 'throw' (SomeoneWrongOnInternet problem)
        'Right' value -> 'pure' value
    ...
@

this works rather nicely especially when you're doing lots of parsing; small
things that can fail but it's all pure code. In conjunction with the 'Either'
monad you can quickly work through getting the values you need knowing it will
fail fast if something goes wrong and you can get an appropriate error message
back to the surface (in our case the 'Program' @τ@ monad) and you can 'throw'
from there.
-}
module Core.Program.Exceptions where

import Control.Exception qualified as Base (
    Exception,
 )
import Control.Exception.Safe qualified as Safe (
    bracket,
    catch,
    finally,
    onException,
    throw,
    try,
 )
import Core.Program.Context (
    Program,
 )

{- |
Catch an exception.

Some care must be taken. Remember that even though it is constrained by the
'Exception' typeclass, @ε@ does /not/ stand for \"any\" exception type; is has
a concrete type when it gets to being used in your code. Things are fairly
straight-forward if you know exactly the exception you are looking for:

@
    'catch'
        action
        (\\(e :: FirstWorldProblem) -> do
            ...
        )
@

but more awkward when you don't.

If you just need to catch all exceptions, the pattern for that is as follows:

@
    'catch'
        action
        (\\(e :: SomeException) -> do
            ...
        )
@

The 'Control.Exception.SomeException' type is the root type of all exceptions;
or rather, all types that have an instance of 'Control.Exception.Exception'
can be converted into this root type. Thus you /can/ catch all synchronous
exceptions but you can't tell which type of exception it was originally; you
rely on the 'Show' instance (which is the default that
'Control.Exception.displayException' falls back to) to display a message which
will hopefully be of enough utility to figure out what the problem is. In
fairness it usually is. (This all seems a bit of a deficiency in the
underlying exception machinery but it's what we have)

This 'catch' function will /not/ catch asynchonous exceptions. If you need to
do that, see the more comprehensive exception handling facilities offered by
__safe-exceptions__, which in turn builds on __exceptions__ and __base__).
Note that 'Program' implements 'Control.Monad.Catch.MonadCatch' so you can use
the full power available there if required.

@since 0.5.0
-}
catch :: Base.Exception ε => Program τ α -> (ε -> Program τ α) -> Program τ α
catch = Safe.catch

{- |
Catch an exception. Instead of handling an exception in a supplied function,
however, return from executing the sub-program with the outcome in an
'Either', with the exception being on the 'Left' side if one is thrown. If the
sub-program completes normally its result is in the 'Right' side.

(this is a wrapper around calling __safe-exceptions__\'s
'Control.Exceptions.Safe.try' function, which in turn wraps __exceptions__\'s
'Control.Exceptions.try', which...)

@since 0.5.0
-}
try :: Base.Exception ε => Program τ α -> Program τ (Either ε α)
try = Safe.try

{- |
Throw an exception.

This will be thrown as a normal synchronous exception that can be caught with
'catch' or 'try' above.

(experienced users will note that 'Program' implements
'Control.Monad.Catch.MonadThrow' and as such this is just a wrapper around
calling __safe-exceptions__'s 'Control.Exceptions.Safe.throw' function)

@since 0.5.0
-}
throw :: Base.Exception ε => ε -> Program τ α
throw = Safe.throw

{- |
Acquire a resource, use it, then release it back.

The bracket pattern is common in Haskell for getting a resource @ρ@ needed for
a computation, preforming that computation, then returning the resource back
to the system. Common examples are when making database connections and doing
file or network operations, where you need to make sure you "close" the
connection afterward before continuing the program so that scare resources
like file handles are released.

Typically you have an open and close action that return and take a resource
respectively, so you can use those directly, and use a lambda in the third
action to actally get at the resource and do something with it when you need
it:

@
    'bracket'
        (openConnection)
        (closeConnection)
        (\c -> do
            this
            thatAndNow
            theOtherThingThatNeeds c
        )
@

Note that 'bracket' does /not/ catch the exception if one is thrown! The
finalizer will run, but then the exception will continue to propogate its way
out of your program's call stack. Note also that the result of the cleanup
action @γ@ is ignored (it can be @()@ or anythign else; it will be discarded).

@since 0.5.0
-}
bracket :: Program τ ρ -> (ρ -> Program τ γ) -> (ρ -> Program τ α) -> Program τ α
bracket = Safe.bracket

{- |
Run an action and then, run a finalizer afterwards. The second action will run
whether or not an exception was raised by the first one. This is like
'bracket' above, but can be used when you know you have cleanup steps to take
after your computation which /do/ have to be run even if (especially if!) an
exception is thrown but that that cleanup doesn't depend on the result of that
computation or the resources used to do it. The return value @γ@ of the
subsequent action is ignored.

@since 0.5.0
-}
finally :: Program τ α -> Program τ γ -> Program τ α
finally = Safe.finally

{- |
Run an action and then, if an exception was raised (and only if an exception
was raised), run the second action. The return value @γ@ of the subsequent
action is is ignored.

@since 0.5.0
-}
onException :: Program τ α -> Program τ γ -> Program τ α
onException = Safe.onException
