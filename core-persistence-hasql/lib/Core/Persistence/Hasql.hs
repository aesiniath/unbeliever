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
import Hasql.Pool (Pool, UsageError (ConnectionUsageError, SessionUsageError), use)
import Hasql.Session (QueryError (QueryError), sql, statement)
import Hasql.Statement (Statement)
import qualified System.Timeout as Base (timeout)

{- |
In the event of a problem connecting to the database or executing the query
this exception will be thrown.
-}
data DatabaseFailure
    = DatabaseConnectionFailed Rope
    | DatabaseQueryFailed Rope
    | DatabaseQueryTimeout
    deriving (Show)

instance Exception DatabaseFailure

{- |
Indicate that the program's top-level application state type contains a
connection pool. This is used (and shared) by all the database connections
being made by this helper library.
-}
class Database δ where
    connectionPoolFrom :: δ -> Pool

--
-- We need to ensure that database connections are aborted if they run too
-- long; otherwise huge database CPU resources are consumed for no benefit to
-- the end user. There are three magic numbers related to timeouts. Not
-- visible here is the default 60 second timeout that e.g. Amazaon Application
-- Load Balancers enforce on HTTP connections. We thus choose a slightly
-- shorter "statement timeout" of 57 seconds after which the databse
-- connection is aborted. Finally, just within that at 55 seconds is an I/O
-- timeout on the Haskell side which is done explicitly so that we can
-- [attempt to] kill the request and more importantly throw an appropriate
-- error (so you can in turn return an appropriate HTTP status code, i.e.
-- 524 A Timeout Occurred so that it's overtly visible within your outer
-- monitoring).
--

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
            Base.timeout (55 * 1000000) $ do
                use
                    pool
                    ( do
                        sql "SET statement_timeout = '57s';"
                        statement values query
                    )

        case result of
            Nothing -> do
                throwTimeout
            Just (Left problem) -> do
                throwErrors label problem
            Just (Right rows) -> do
                telemetry
                    [ metric "result_count" (length rows :: Int)
                    ]
                pure (fmap f rows)

throwErrors :: Rope -> UsageError -> Program δ a
throwErrors message result = do
    case result of
        ConnectionUsageError connectionError -> do
            warn message

            telemetry
                [ metric "error" ("Database connection problem: " <> intoRope (show connectionError))
                ]
            throw (DatabaseConnectionFailed message)
        SessionUsageError (QueryError template parameters commandError) -> do
            warn message
            debugS "template" template
            debugS "parameters" parameters

            telemetry
                [ metric "error" ("Database transaction failed: " <> intoRope (show commandError))
                ]
            throw (DatabaseQueryFailed message)

throwTimeout :: Program δ a
throwTimeout = do
    let message = "Database query timeout"
    warn message
    telemetry
        [ metric "error" message
        ]
    throw DatabaseQueryTimeout

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
