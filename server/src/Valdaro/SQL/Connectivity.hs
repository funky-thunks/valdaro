{-# LANGUAGE OverloadedStrings #-}

module Valdaro.SQL.Connectivity
  ( CanSQL
  , runDB
  , withConnection

  , testConnection
  ) where

import           Control.Exception            (mask)
import           Control.Exception.Base       (onException)
import           Control.Monad.Trans.Control  (MonadBaseControl, control)
import           Data.Pool                    (Pool, destroyResource,
                                               putResource, takeResource)
import           Database.PostgreSQL.Simple   (Connection, Only)
import qualified Database.PostgreSQL.Transact as DBT
import           Prelude                      hiding (local)
import           Valdaro.SQL.Configuration

type CanSQL r m = (MonadReader r m, HasSQLRuntime r, MonadBaseControl IO m, MonadIO m)

runDB :: CanSQL r m => DBT.DBT m result -> m result
runDB action = withConnection (DBT.runDBTSerializable action)

withConnection :: CanSQL r m => (Connection -> m result) -> m result
withConnection action = do
  SQLRuntime { pool } <- asks getSQLRuntime
  withResource pool action

testConnection :: CanSQL r m => m Bool
testConnection =
  let check :: [Only Int] -> Bool
      check rows = length rows == 1
   in runDB $ check <$> DBT.query_ "SELECT 1;"

withResource ::
    (MonadBaseControl IO m)
  => Pool a -> (a -> m b) -> m b
{-# SPECIALIZE withResource :: Pool a -> (a -> IO b) -> IO b #-}
withResource pool act = control $ \runInIO -> mask $ \restore -> do
  (resource, local) <- takeResource pool
  ret <- restore (runInIO (act resource)) `onException`
            destroyResource pool local resource
  putResource local resource
  return ret
{-# INLINABLE withResource #-}
