{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Valdaro.Service
  ( Service(..)
  , runService
  , runMigrations
  ) where

import qualified Data.Text.IO                         as T
import           Network.Wai.Handler.Warp             (runEnv)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant.Server                       hiding (toApplication)
import qualified Valdaro.SQL                          as SQL

data Service environment =
  Service { serviceName    :: Text
          , getEnvironment :: IO environment
          , toApplication  :: environment -> Application
          , migrations     :: [ SQL.MigrationCommand ]
          }

runService :: SQL.HasSQLRuntime environment => Service environment -> IO ()
runService svc@Service { .. } = do
  T.putStrLn $ "Starting " <> serviceName
  getEnvironment >>= check svc <> start . toApplication

runMigrations :: SQL.HasSQLRuntime environment => Service environment -> Maybe FilePath -> IO ()
runMigrations Service {..} fixtures =
  let sql = SQL.applyMigrationsAndFixtures migrations fixtures
   in runOrDie sql "Setting up SQL schema" =<< getEnvironment

check :: SQL.HasSQLRuntime environment => Service environment -> environment -> IO ()
check Service { .. } =
  fold
    [ runOrDie  SQL.testConnection                 "SQL test connection"
    , runOrDie (SQL.validateMigrations migrations) "Validating SQL migrations"
    ]

runOrDie :: ReaderT r IO Bool -> String -> r -> IO ()
runOrDie action message environment = do
  results <- runReaderT action environment
  unless results $
    die $ message <> "failed... Aborting."

start :: Application -> IO ()
start application = do
  let defaultPort = 8080
  port <- fromMaybe (show defaultPort) <$> lookupEnv "PORT"
  putStrLn $ "Listening :" <> port
  runEnv defaultPort (logStdout application)
