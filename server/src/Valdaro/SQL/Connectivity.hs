{-# LANGUAGE OverloadedStrings #-}

module Valdaro.SQL.Connectivity
  ( CanSQL
  , runDB
  , withConnection

  , testConnection
  ) where

import           Control.Monad.IO.Unlift      (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Pool                    (withResource)
import           Database.PostgreSQL.Simple   (Connection, Only)
import qualified Database.PostgreSQL.Transact as DBT
import           Valdaro.SQL.Configuration

type CanSQL r m = (MonadReader r m, HasSQLRuntime r, MonadBaseControl IO m, MonadIO m, MonadUnliftIO m)

runDB :: CanSQL r m => DBT.DBT m result -> m result
runDB action = withConnection (DBT.runDBTSerializable action)

withConnection :: CanSQL r m => (Connection -> m result) -> m result
withConnection action = do
  SQLRuntime { pool } <- asks getSQLRuntime
  withRunInIO $ \ runInIO ->
    withResource pool (runInIO . action)

testConnection :: CanSQL r m => m Bool
testConnection =
  let check :: [Only Int] -> Bool
      check rows = length rows == 1
   in runDB $ check <$> DBT.query_ "SELECT 1;"
