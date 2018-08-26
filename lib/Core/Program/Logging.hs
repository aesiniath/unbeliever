{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Program.Logging
    (
        putMessage
      , getConsoleWidth
      , processStandardOutput
      , processDebugMessages
    ) where

import Chrono.TimeStamp (TimeStamp(..), getCurrentTimeNanoseconds)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (forever)
import qualified Data.ByteString as S (pack, hPut)
import qualified Data.ByteString.Char8 as C (singleton)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Fixed
import Data.Hourglass (timePrint, TimeFormatElem(..))
import System.Console.Terminal.Size (Window(..), size, hSize)
import Time.System (timezoneCurrent)

import Core.Text
import Core.Render
import Core.System
import Core.Program.Context

{-
class Monad m => MonadLog a m where
    logMessage :: Monoid a => Severity -> a -> m () 

instance MonadLog Text IO where
    logMessage severity message = do
        tick <- getCurrentTimeNanoseconds
        
        let line = show tick ++ " [" ++ show severity ++ "] " ++ show message
        hPutStrLn stdout line
-}

putMessage :: Context -> Message -> IO ()
putMessage context message@(Message now nature text potentialValue) = do
    let start = startTimeFrom context
    let width = terminalWidthFrom context
    let output = outputChannelFrom context
    let logger = loggerChannelFrom context

    now <- getCurrentTimeNanoseconds

    let display = case potentialValue of
            Just value ->
                if contains '\n' value
                    then text <> " =\n" <> value
                    else text <> " = " <> value
            Nothing -> text

    let result = formatLogMessage start now display

    atomically $ do
        writeTChan output result
        writeTChan logger message


formatLogMessage :: TimeStamp -> TimeStamp -> Text -> Text
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
        [ intoText stampZ
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
padWithZeros :: Int -> String -> Text
padWithZeros digits str =
    intoText (pad ++ str)
  where
    pad = take len (replicate digits '0')
    len = digits - length str


processDebugMessages :: TChan Message -> IO ()
processDebugMessages logger = do
    forever $ do
        Message now severity text potentialValue <- atomically (readTChan logger)
        
        return ()

getConsoleWidth :: IO (Int)
getConsoleWidth = do
    window <- size
    let width =  case window of
            Just (Window _ w) -> w
            Nothing -> 80
    return width

processStandardOutput :: TChan Text -> IO ()
processStandardOutput output = do
    forever $ do
        text <- atomically (readTChan output)

        S.hPut stdout (fromText text)
        S.hPut stdout (C.singleton '\n')

