{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Reading and writing data to and from a Postgres database.

This module simply contains conveniences for using the most excellent
__hasql__ library in concert with the rest of the packages in
this collection.
-}
module Core.Persistence.Hasql (
    performQuerySingleton,
    performQueryVector,
) where

import Core.Program.Context
import Core.Program.Execute
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
import Data.Vector (Vector, toList)
import Hasql.Pool (Pool, UsageError (ConnectionError, SessionError), use)
import Hasql.Session (statement)
import Hasql.Statement (Statement)

data DatabaseFailure = DatabaseFailure Rope
    deriving (Show)

{- |
Indicate that the program's top-level application state type contains a
connection pool. This is used (and shared) by all the database connections
being made by this helper library.
-}
class Database δ where
    connectionPoolFrom :: δ -> Pool


performQueryActual ::
    Database δ =>
    Functor f =>
    Rope ->
    (result -> α) ->
    (f α -> g α) ->
    params ->
    Statement params (f result) ->
    Program δ (g α)
performQueryActual label f g values query = do
    encloseSpan label $ do
        state <- getApplicationState
        let pool = connectionPoolFrom state

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                throw problem
            Right rows ->
                pure $ g $ fmap f rows

performQuerySingleton ::
    Database δ =>
    Rope ->
    (result -> α) ->
    params ->
    Statement params result ->
    Program δ α
performQuerySingleton label f values query = do
    encloseSpan label $ do
        state <- getApplicationState
        let pool = connectionPoolFrom state

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                case problem of
                    ConnectionError e -> do
                        telemetry
                            [ metric "error" (packRope (show e))
                            ]
                        throw Boom
                    SessionError e -> do
                        telemetry
                            [ metric "error" (packRope (show problem))
                            ]
                        throw e
            Right row ->
                pure (f row)

performQueryVector ::
    Database δ =>
    Rope ->
    (result -> α) ->
    params ->
    Statement params (Vector result) ->
    Program δ [α]
performQueryVector label f values query = do
    encloseSpan label $ do
        state <- getApplicationState
        let pool = connectionPoolFrom state

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                case problem of
                    ConnectionError e -> do
                        telemetry
                            [ metric "error" (packRope (show e))
                            ]
                        throw Boom
                    SessionError e -> do
                        telemetry
                            [ metric "error" (packRope (show problem))
                            ]
                        throw e
            Right rows ->
                pure
                    ( fmap
                        f
                        (toList rows)
                    )
    