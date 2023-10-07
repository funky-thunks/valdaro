{-# LANGUAGE LambdaCase #-}

module Valdaro.CLI
  ( bootstrap
  ) where

import           Valdaro.CLI.Options (Command (..), readCommand)
import           Valdaro.Service     (Service, runMigrations, runService)
import           Valdaro.SQL         (HasSQLRuntime)

interpretCommand :: HasSQLRuntime environment => Service environment -> Command -> IO ()
interpretCommand svc = \case
  Migrate fixtures -> runMigrations svc fixtures
  Serve            -> runService    svc

bootstrap :: HasSQLRuntime environment => Service environment -> IO ()
bootstrap svc = interpretCommand svc =<< readCommand
