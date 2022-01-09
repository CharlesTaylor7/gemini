module Main where

import Relude

import Polysemy
import Polysemy.Error

import Variant

import Hasql.Migration
import Hasql.Session (Session)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel(RepeatableRead), Mode (Write))
import Hasql.Transaction.Sessions qualified as Hasql

import Rel8.Polysemy (ConnectionError, QueryError, runSession, acquireHasqlConnection)
import MarsRover.Env (loadEnv)


migration :: Members [Embed Transaction, Error MigrationError] r => MigrationCommand -> Sem r ()
migration cmd = do
  maybeError <- embed $ runMigration cmd
  maybe pass throw maybeError

runTransaction :: Member (Embed Session) r => Sem (Embed Transaction : r) a -> Sem r a
runTransaction = interpret $ \case
  Embed t -> embed $ Hasql.transaction RepeatableRead Write t


main :: IO ()
main = do
  -- read .env file
  _ <- loadEnv

  -- load file
  cmds <- loadMigrationsFromDirectory "migrations"

  -- run migrations
  traverse_ migration (MigrationInitialization : cmds)
    & runTransaction
    & runSession
    & acquireHasqlConnection
    & errorToVariant @MigrationError
    & errorToVariant @ConnectionError
    & errorToVariant @QueryError
    & runErrorStdoutIOFinal @(V [MigrationError, ConnectionError, QueryError])
    & embedToFinal
    & runFinal
