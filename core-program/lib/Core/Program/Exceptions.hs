{-# LANGUAGE ImportQualifiedPost #-}

module Core.Program.Exceptions where

import Control.Exception qualified as Base (
    Exception,
 )
import Control.Exception.Safe qualified as Safe (
    catch,
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

@since 0.4.7
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
