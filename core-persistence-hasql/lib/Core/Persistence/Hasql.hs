{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Reading and writing data to and from a Postgres database.

This module simply contains conveniences for using the most excellent
__hasql__ library in concert with the rest of the packages in
this collection.
-}
module Core.Persistence.Hasql (
    ) where

import Core.Program.Context
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
import Data.Vector (Vector, toList)
import Hasql.Pool (UsageError (ConnectionError, SessionError), use)
import Hasql.Session (statement)
import Hasql.Statement (Statement)

data DatabaseFailure = DatabaseFailure Rope
    deriving (Show)

performQuerySingleton ::
    Rope ->
    (result -> a) ->
    params ->
    Statement params result ->
    Program z a
performQuerySingleton label f values query = do
    encloseSpan label $ do
        settings <- getApplicationState
        let pool = settingsConfigPool settings

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                case problem of
                    ConnectionError e1 -> do
                        telemetry
                            [ metric "error" (packRope (show e1))
                            ]
                        throw Boom
                    SessionError e2 -> do
                        telemetry
                            [ metric "error" (packRope (show problem))
                            ]
                        throw e2
            Right row ->
                pure (f row)

performQueryVector ::
    Rope ->
    (result -> a) ->
    params ->
    Statement params (Vector result) ->
    Program z [a]
performQueryVector label f values query = do
    encloseSpan label $ do
        settings <- getApplicationState
        let pool = settingsConfigPool settings

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                case problem of
                    ConnectionError e1 -> do
                        telemetry
                            [ metric "error" (packRope (show e1))
                            ]
                        throw Boom
                    SessionError e2 -> do
                        telemetry
                            [ metric "error" (packRope (show problem))
                            ]
                        throw e2
            Right rows ->
                pure
                    ( fmap
                        f
                        (toList rows)
                    )
