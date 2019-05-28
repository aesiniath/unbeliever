{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Output and Logging from your program.

Broadly speaking, there are two kinds of program: console tools invoked for
a single purpose, and long-running daemons that effectively run forever.

Tools tend to be run to either have an effect (in which case they tend not
to a say much of anything) or to report a result. This tends to be written
to \"standard output\"—traditionally abbreviated in code as @stdout@—which
is usually printed to your terminal.

Daemons, on the other hand, don't write their output to file descriptor 1;
rather they tend to respond to requests by writing to files, replying over
network sockets, or sending up smoke signals (@ECPUTOOHOT@, in case you're
curious). What daemons /do/ output, however, is log messages.

While there are many sophisticated logging services around that you can
interact with directly, from the point of view of an individual /program/
these tend to have faded away and have become more an aspect of the
Infrastructure- or Platform-as-a-Service you're running on. Over the past
few years containerization mechanisms like __docker__, then more recently
container orchestration layers like __kubernetes__, have generally simply
captured programs' standard output /as if it were the program's log output/
and then sent that down external logging channels to whatever log analysis
system is available. Even programs running locally under __systemd__ or
similar tend to follow the same pattern; services write to @stdout@ and
that output, as "logs", ends up being fed to the system journal.

So with that in mind, in your program you will either be outputting results
to @stdout@ or not writing there at all, and you will either be describing
extensively what your application is up to, or not at all. 

There is also a \"standard error\" file descriptor available. We recommend
not using it. At best it is unclear what is written to @stderr@ and what
isn't; at worse it is lost as many environments in the wild discard
@stderr@ entirely. To avoid this most of the time people just combine them
in the invoking shell with @2>&1@, which inevitably results in @stderr@
text appearing in the middle of normal @stdout@ lines corrupting them.

The original idea of standard error was to provde a way to report adverse
conditions without interrupting normal text output, but as we have just
observed if it happens without context or out of order there isn't much
point. Instead this library offers a mechanism which caters for the
different /kinds/ of output in a unified, safe manner.

== Three kinds of output/logging messages

/Standard output/

Your program's normal output to the terminal. This library provides the
'write' (and 'writeS' and 'writeR') functions to send output to @stdout@.

/Events/

When running a tool, you sometimes need to know /what it is doing/ as it is
carrying out its steps. The 'event' function allows you to emit descriptive
messages to the log channel tracing the activities of your program.

Ideally you would never need to turn this on in a command-line tool, but
sometimes a user or operations engineer needs to see what an application is
up to. These should be human readable status messages to convey a sense of
progress.

In the case of long-running daemons, 'event' can be used to describe
high-level lifecycle events, to document individual requests, or even
describing individual transitions in a request handler's state machine, all
depending on the nature of your program.

/Debugging/

Programmers, on the other hand, often need to see the internal state of
the program when /debugging/.

You almost always you want to know the value of some variable or parameter,
so the 'debug' (and 'debugS' and 'debugR') utility functions here send
messages to the log channel prefixed with a label that is, by convention,
the name of the value you are examining.

The important distinction here is that such internal values are almost
never useful for someone other than the person or team who wrote the code
emitting it. Operations engineers might be asked by developers to turn on
@--debug@ing and report back the results; but a user of your program is not
going to do that in and of themselves to solve a problem.

== Single output channel

It is the easy to make the mistake of having multiple subsystems attempting
to write to @stdout@ and these outputs corrupting each other, especially in
a multithreaded language like Haskell. The output actions described here
send all output to terminal down a single thread-safe channel. Output will
be written in the order it was executed, and (so long as you don't use the
@stdout@ Handle directly yourself) your terminal output will be sound.

Passing @--verbose@ on the command-line of your program will cause 'event'
to write its tracing messages to the terminal. This shares the same output
channel as the 'write'@*@ functions and will /not/ cause corruption of your
program's normal output.

Passing @--debug@ on the command-line of your program will cause the
'debug'@*@ actions to write their debug-level messages to the terminal.
This shares the same output channel as above and again will not cause
corruption of your program's normal output.

== Logging channel

/Event and debug messages are internally also sent to a "logging channel",/
/as distinct from the "output" one. This would allow us to send them/
/directly to a file, syslog, or network logging service, but this is/
/as-yet unimplemented./
-}
module Core.Program.Logging
    (
        putMessage
      , Verbosity
        {-* Normal output -}
      , write
      , writeS
      , writeR
        {-* Event tracing -}
      , event
        {-* Debugging -}
      , debug
      , debugS
      , debugR
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.Reader.Class (MonadReader(ask))
import Data.Fixed
import Data.Hourglass (timePrint, TimeFormatElem(..))
import qualified Data.Text.Short as S (replicate)

import Core.Text.Rope
import Core.Text.Utilities
import Core.System.Base
import Core.Program.Context

{-
class Monad m => MonadLog a m where
    logMessage :: Monoid a => Severity -> a -> m () 
-}

putMessage :: Context τ -> Message -> IO ()
putMessage context message@(Message now _ text potentialValue) = do
    let start = startTimeFrom context
    let output = outputChannelFrom context
    let logger = loggerChannelFrom context

    let display = case potentialValue of
            Just value ->
                if containsCharacter '\n' value
                    then text <> " =\n" <> value
                    else text <> " = " <> value
            Nothing -> text

    let result = formatLogMessage start now display

    atomically $ do
        writeTQueue output result
        writeTQueue logger message


formatLogMessage :: TimeStamp -> TimeStamp -> Rope -> Rope
formatLogMessage start now message =
  let
    start' = unTimeStamp start
    now' = unTimeStamp now
    stampZ = timePrint
        [ Format_Hour
        , Format_Text ':'
        , Format_Minute
        , Format_Text ':'
        , Format_Second
        , Format_Text 'Z'
        ] now

    -- I hate doing math in Haskell
    elapsed = fromRational (toRational (now' - start') / 1e9) :: Fixed E3
  in
    mconcat
        [ intoRope stampZ
        , " ("
        , padWithZeros 9 (show elapsed)
        , ") "
        , message
        ]

--
-- | Utility function to prepend \'0\' characters to a string representing a
-- number.
--
{-
    Cloned from **locators** package Data.Locators.Hashes, BSD3 licence
-}
padWithZeros :: Int -> String -> Rope
padWithZeros digits str =
    intoRope pad <> intoRope str
  where
    pad = S.replicate len "0"
    len = digits - length str

{-|
Write the supplied text to @stdout@.

This is for normal program output.

@
     'write' "Beginning now"
@
-}
write :: Rope -> Program τ ()
write text = do
    context <- ask
    liftIO $ do
        let out = outputChannelFrom context

        !text' <- evaluate text
        atomically (writeTQueue out text')

{-|
Call 'show' on the supplied argument and write the resultant text to
@stdout@.

(This is the equivalent of 'print' from __base__)
-}
writeS :: Show α => α -> Program τ ()
writeS = write . intoRope . show

{-|
Pretty print the supplied argument and write the resultant text to
@stdout@. This will pass the detected terminal width to the 'render'
function, resulting in appopriate line wrapping when rendering your value.
-}
writeR :: Render α => α -> Program τ ()
writeR thing = do
    context <- ask
    liftIO $ do
        let out = outputChannelFrom context
        let columns = terminalWidthFrom context

        let text = render columns thing
        !text' <- evaluate text
        atomically (writeTQueue out text')

{-|
Note a significant event, state transition, status, or debugging
message. This:

@
    'event' "Starting..."
@

will result in

> 13:05:55Z (0000.001) Starting...

appearing on stdout /and/ the message being sent down the logging
channel. The output string is current time in UTC, and time elapsed
since startup shown to the nearest millisecond (our timestamps are to
nanosecond precision, but you don't need that kind of resolution in
in ordinary debugging).

Messages sent to syslog will be logged at @Info@ level severity.
-}
event :: Rope -> Program τ ()
event text = do
    context <- ask
    liftIO $ do
        level <- readMVar (verbosityLevelFrom context)
        when (isEvent level) $ do
            now <- getCurrentTimeNanoseconds
            putMessage context (Message now Event text Nothing)

isEvent :: Verbosity -> Bool
isEvent level = case level of
    Output -> False
    Event  -> True
    Debug  -> True

isDebug :: Verbosity -> Bool
isDebug level = case level of
    Output -> False
    Event  -> False
    Debug  -> True

{-|
Output a debugging message formed from a label and a value. This is like
'event' above but for the (rather common) case of needing to inspect or
record the value of a variable when debugging code.  This:

@
    'setProgramName' \"hello\"
    name <- 'getProgramName'
    'debug' \"programName\" name
@

will result in

> 13:05:58Z (0003.141) programName = hello

appearing on stdout /and/ the message being sent down the logging channel,
assuming these actions executed about three seconds after program start.

Messages sent to syslog will be logged at @Debug@ level severity.
-}
debug :: Rope -> Rope -> Program τ ()
debug label value = do
    context <- ask
    liftIO $ do
        level <- readMVar (verbosityLevelFrom context)
        when (isDebug level) $ do
            now <- getCurrentTimeNanoseconds
            !value' <- evaluate value
            putMessage context (Message now Debug label (Just value'))

{-|
Convenience for the common case of needing to inspect the value
of a general variable which has a 'Show' instance
-}
debugS :: Show α => Rope -> α -> Program τ ()
debugS label value = debug label (intoRope (show value))

{-|
Convenience for the common case of needing to inspect the value of a
general variable for which there is a 'Render' instance and so can pretty
print the supplied argument to the log. This will pass the detected
terminal width to the 'render' function, resulting in appopriate line
wrapping when rendering your value (if logging to something other than
console the default width of @80@ will be applied).
-}
debugR :: Render α => Rope -> α -> Program τ ()
debugR label thing = do
    context <- ask
    liftIO $ do
        level <- readMVar (verbosityLevelFrom context)
        when (isDebug level) $ do
            now <- getCurrentTimeNanoseconds

            let columns = terminalWidthFrom context

            -- TODO take into account 22 width already consumed by timestamp
            -- TODO move render to putMessage? putMessageR?
            let value = render columns thing
            !value' <- evaluate value
            putMessage context (Message now Debug label (Just value'))

