{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Valdaro.SQL.Configuration
  ( Configuration(..)
  , SQLRuntime(..)
  , HasSQLRuntime(..)

  , readConfiguration
  , buildSQLRuntime
  ) where

import           Data.Pool                  (Pool, createPool)
import           Database.PostgreSQL.Simple (Connection, close,
                                             connectPostgreSQL)
import           Valdaro.Configuration

data Configuration =
  Configuration
    { connectionUrl :: ByteString
    , passwordFile  :: Maybe FilePath
    , maxPoolSize   :: Int
    }

readConfiguration :: Env Configuration
readConfiguration =
  Configuration
    <$> getEnv      "POSTGRESQL_CONNECTION_URL"
    <*> maybeGetEnv "POSTGRESQL_PASSWORD_FILE"
    <*> readEnvOr   "POSTGRESQL_MAX_POOL_SIZE" 5

newtype SQLRuntime =
  SQLRuntime
    { pool :: Pool Connection
    } deriving stock (Show)

buildSQLRuntime :: Configuration -> IO SQLRuntime
buildSQLRuntime Configuration { .. } = do
  password <- join <$> traverse readPassword passwordFile
  let connect =
        connectPostgreSQL $ case password of
          Nothing -> connectionUrl
          Just p  -> connectionUrl <> " password=" <> p
  SQLRuntime <$> createPool connect close 1 2.0 maxPoolSize

class HasSQLRuntime env where
  getSQLRuntime :: env -> SQLRuntime

instance HasSQLRuntime SQLRuntime where
  getSQLRuntime = id

instance HasSQLRuntime env => HasSQLRuntime (Sensitive env) where
  getSQLRuntime (Sensitive e) = getSQLRuntime e
