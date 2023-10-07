module Valdaro.CLI.Options
  ( readCommand
  , Command(..)
  ) where

import           Options.Applicative

data Command
  = Serve
  | Migrate (Maybe FilePath)

readCommand :: IO Command
readCommand =
  execParser (info (commandsParser <**> helper) (fullDesc <> progDesc "backoffice backend service"))

commandsParser :: Parser Command
commandsParser =
  subparser $
       command "serve"   (info (pure Serve)  (progDesc "Start the web service"))
    <> command "migrate" (info migrateParser (progDesc "Apply SQL migrations"))

migrateParser :: Parser Command
migrateParser =
  Migrate <$> optional fixturesOptionParser

fixturesOptionParser :: Parser FilePath
fixturesOptionParser =
  strOption $
       long "fixtures"
    <> help "Path to a SQL file to be executed after migrations. Useful for testing purposes."
