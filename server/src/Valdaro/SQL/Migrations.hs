module Valdaro.SQL.Migrations
  ( applyMigrations
  , validateMigrations
  , script
  , module X
  ) where

import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationResult (..),
                                                       ScriptName,
                                                       defaultOptions,
                                                       runMigrations)
import qualified Database.PostgreSQL.Simple.Migration as X (MigrationCommand (..))
import           Database.PostgreSQL.Simple.Types     (Query (..))
import           Valdaro.SQL.Connectivity             (CanSQL, withConnection)

script :: ScriptName -> Query -> MigrationCommand
script sn = MigrationScript sn . fromQuery

applyMigrations :: CanSQL r m => [MigrationCommand] -> m Bool
applyMigrations =
  run "Migrations succeeded" "Migrations failed"

validateMigrations :: CanSQL r m => [MigrationCommand] -> m Bool
validateMigrations =
  run "Migrations validation succeeded" "Migrations validation failed"
     . fmap MigrationValidation

run :: CanSQL r m => String -> String -> [MigrationCommand] -> m Bool
run successMessage failureMessage migrations =
  withConnection $ \connection -> liftIO $ do
    result <- runMigrations connection defaultOptions (MigrationInitialization : migrations)
    case result of
      MigrationSuccess -> putStrLn successMessage $> True
      MigrationError _ -> putStrLn failureMessage $> False
