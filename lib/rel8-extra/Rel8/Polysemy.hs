{-# language TemplateHaskell #-}
module Rel8.Polysemy
  ( DBCommand(..)
  , runDBCommand
  , runSession
  , selectCmd, insertCmd, updateCmd, deleteCmd
  , acquireHasqlConnection
  , Connection
  , ConnectionError
  , QueryError
  , Session
--  , module R
  )
  where

import Relude
import Polysemy
import Polysemy.Input
import Polysemy.Error
import qualified Rel8 as R

import Hasql.Connection as Hasql hiding (ConnectionError)
import Hasql.Session as Hasql
import Optics (_Left, over)


-- | An effect for interpreting rel8 queries against postgres
data DBCommand m a where
  SelectCmd :: R.Serializable exprs a => R.Query exprs -> DBCommand m [a]
  InsertCmd :: R.Insert a -> DBCommand m a
  UpdateCmd :: R.Update a -> DBCommand m a
  DeleteCmd :: R.Delete a -> DBCommand m a


makeSem  ''DBCommand

-- | effect handler interprets commands in Hasql db session via rel8
runDBCommand :: Member (Embed Session) r => Sem (DBCommand : r) a -> Sem r a
runDBCommand = interpret $ \case
  SelectCmd query -> toSession $ R.select query
  InsertCmd args -> toSession $ R.insert args
  UpdateCmd args -> toSession $ R.update args
  DeleteCmd args -> toSession $ R.delete args
  where toSession = embed . Hasql.statement ()


-- | interprets hasql sessions into their 3 constituent effects: IO, Input connection and possible error
runSession
  :: 
    ( MonadIO m
    , Members
      '[ Embed m
       , Error Hasql.QueryError
       ] r
    )
  => Sem (Embed Session : r) a
  -> Sem (Input Hasql.Connection:r) a
runSession = reinterpret $ \case
  Embed session -> do
    connection <- input
    possibleResult <- embed $ liftIO $ Hasql.run session connection
    fromEither possibleResult


newtype ConnectionError = ConnectionError (Maybe ByteString)
  deriving stock (Show)


acquireHasqlConnection
  :: 
    ( MonadIO m
    , Members '[Embed m, Error ConnectionError] r
    )
  => Sem (Input Hasql.Connection : r) a
  -> Sem r a
acquireHasqlConnection val = do
  possibleConn <- embed $ liftIO $ Hasql.acquire "" <&> over _Left ConnectionError
  conn <- fromEither possibleConn
  runInputConst conn val
