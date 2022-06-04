{-# LANGUAGE ImportQualifiedPost #-}

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

This will /not/ catch asynchonous exceptions. If you need to that, see the
more comprehensive exception handling facilities offered by
__safe-exceptions__, which in turn builds on __exceptions__ and __base__).
Note that 'Program' implements 'MonadCatch' so you can use the full power
available there if required.

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

(experienced users will note that 'Program' implements 'MonadThrow' and as
such this is just a wrapper around calling __safe-exceptions__'s
'Control.Exceptions.Safe.throw' function)

@since 0.5.0
-}
throw :: Base.Exception ε => ε -> Program τ α
throw = Safe.throw


{- |
Acquire a resource, use it, then release it back.

The bracket pattern is common in Haskell for getting a resource @ρ@ needed for
a computation, preforming that computation, then releasing the resource back
to the system. Common examples are when making database connections and doing
file or network operations, where you need to make sure you "close" the
connection afterward before continuing the program so that scare resources
like file handles are released.

Note that this does /not/ catch the exception if one is thrown! The finalizer
will run, but then the exception will continue to propogate its way out of
your program's call stack. Note also that the result of the cleanup action @γ@
is ignored.

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
computation or the resources used to do it.

@since 0.5.0
-}
finally :: Program τ α -> Program τ β -> Program τ α
finally = Safe.finally

{- |
Run an action and the, if  an exception was raised (and only if), run the
second action.

@since 0.5.0
-}
onException :: Program τ α -> Program τ β -> Program τ α
onException = Safe.onException
