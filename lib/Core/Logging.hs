{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Logging
    ( MonadLog(..)
    , critical
    , warning
    , info
    , debug
    , Severity(..)
    ) where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stdout)
import Chrono.TimeStamp (TimeStamp, getCurrentTimeNanoseconds)

import Core.Text
import Core.Render

class Monad m => MonadLog a m where
    logMessage :: Monoid a => Severity -> a -> m () 

instance MonadLog Text IO where
    logMessage severity message = do
        tick <- getCurrentTimeNanoseconds
        
        let line = show tick ++ " [" ++ show severity ++ "] " ++ show message
        hPutStrLn stdout line

debug :: MonadLog Text m => Text -> m ()
debug t = logMessage Debug t

info :: MonadLog Text m => Text -> m ()
info t = logMessage Info t 

warning :: MonadLog Text m => Text -> m ()
warning t = logMessage Warning t

critical :: MonadLog Text m => Text -> m ()
critical t = logMessage Critical t


data Message = Message Severity Text 

data Severity -- or LogLevel ?
    = Debug 
    | Info
    | Warning
    | Critical
    deriving Show

instance Render Severity where
    render x = case x of
        Debug    -> "[Debug]   "
        Info     -> "[Info]    "
        Warning  -> "[Warning] "
        Critical -> "[Critical]"
