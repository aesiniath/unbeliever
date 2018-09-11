{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Core.Program
    ( configure
    , Config
    , Parameters(..)
    , ParameterValue(..)
    , simple
    , complex
    , LongName(..)
    , ShortName
    , Options(..)
    , Variables(..)
    , Commands(..)
    , Description
    , execute
    , executeWith
    , Program
    , terminate
    , setProgramName
    , getProgramName
    , getCommandLine
    , write
    , writeS
    , event
    , debug
    , debugS
    , fork
    , sleep
    ) where

import Core.Program.Context
import Core.Program.Arguments
import Core.Program.Logging
import Core.Program.Execute

