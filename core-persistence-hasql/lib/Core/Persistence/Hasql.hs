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
    performQueryMaybe,
) where

import Core.Program.Context
import Core.Program.Execute
import Core.Program.Logging
import Core.System.Base
import Core.Telemetry.Observability
import Core.Text.Rope
import Data.Vector (Vector, toList)
import GHC.Tuple (Solo (Solo), getSolo)
import Hasql.Pool (Pool, UsageError (ConnectionError, SessionError), use)
import Hasql.Session (QueryError (QueryError), statement)
import Hasql.Statement (Statement)

{- |
In the event of a problem connecting to the database or executing the query
this exception will be thrown.
-}
data DatabaseFailure
    = DatabaseConnectionFailed Rope
    | DatabaseQueryFailed Rope
    deriving (Show)

instance Exception DatabaseFailure

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
    params ->
    Statement params (f result) ->
    Program δ (f α)
performQueryActual label f values query = do
    encloseSpan label $ do
        state <- getApplicationState
        let pool = connectionPoolFrom state

        result <- liftIO $ do
            use pool (statement values query)

        case result of
            Left problem -> do
                throwErrors label problem
            Right rows ->
                pure (fmap f rows)

throwErrors :: Rope -> UsageError -> Program δ a
throwErrors message result = do
    case result of
        ConnectionError connectionError -> do
            warn message

            telemetry
                [ metric "error" ("Database connection problem: " <> intoRope (show connectionError))
                ]
            throw (DatabaseConnectionFailed message)
        SessionError (QueryError template parameters commandError) -> do
            warn message
            debugS "template" template
            debugS "parameters" parameters

            telemetry
                [ metric "error" ("Database transaction failed: " <> intoRope (show commandError))
                ]
            throw (DatabaseQueryFailed message)

performQuerySingleton ::
    Database δ =>
    Rope ->
    (result -> α) ->
    params ->
    Statement params result ->
    Program δ α
performQuerySingleton label f values query = performQueryActual label f values (fmap Solo query) >>= pure . getSolo

performQueryVector ::
    Database δ =>
    Rope ->
    (result -> α) ->
    params ->
    Statement params (Vector result) ->
    Program δ [α]
performQueryVector label f values query = performQueryActual label f values query >>= pure . toList

performQueryMaybe ::
    Database δ =>
    Rope ->
    (result -> α) ->
    params ->
    Statement params (Maybe result) ->
    Program δ (Maybe α)
performQueryMaybe label f values query = performQueryActual label f values query
