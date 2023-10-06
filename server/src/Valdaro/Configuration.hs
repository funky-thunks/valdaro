{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Valdaro.Configuration
  ( Env
  , EnvError(..)
  , renderEnvError
  , runEnv
  , VarName

  , getEnv
  , readEnv
  , parseEnv
  , maybeGetEnv
  , maybeReadEnv
  , readEnvOr
  , getFlag
  , getFlagOr

  , Sensitive(..)

  , readPassword

  ) where

import           Control.Monad.Validate
import qualified Data.ByteString.Char8  as C8
import           Data.Char              (toLower)
import           GHC.Show               (Show (..))
import           Relude.Extra.Newtype   (un)

type Env = ValidateT [EnvError] IO

data EnvError
  = MissingEnvVar VarName
  | Unparseable   VarName (Maybe String)

newtype VarName = VarName String deriving newtype (IsString)

renderEnvError :: EnvError -> Text
renderEnvError = \case
  MissingEnvVar n   -> "Missing env var "     <> quote (un n)
  Unparseable   n e -> "Unparseable env var " <> quote (un n) <> foldMap (\pe -> ", parse error: " <> quote (fromString pe)) e

quote :: (Semigroup str, IsString str) => String -> str
quote s = "'" <> fromString s <> "'"

runEnv :: MonadIO m => Env result -> m (Either [Text] result)
runEnv = liftIO . runValidateT . mapErrors (fmap renderEnvError)

getEnv :: IsString str => VarName -> Env str
getEnv = fmap fromString . getRawEnv

readEnv :: Read a => VarName -> Env a
readEnv varName = do
  rawValue <- getRawEnv varName
  maybe (refute [Unparseable varName Nothing]) pure (readMaybe rawValue)

parseEnv :: (String -> Either String result) -> VarName -> Env result
parseEnv decoder varName = do
  rawValue <- getRawEnv varName
  case decoder rawValue of
    Right result      -> pure result
    Left  decodeError -> refute [Unparseable varName (Just decodeError)]

getRawEnv :: VarName -> Env String
getRawEnv varName = maybe (refute [MissingEnvVar varName]) pure =<< lookupEnv (un varName)

maybeGetEnv :: IsString str => VarName -> Env (Maybe str)
maybeGetEnv = fmap (fmap fromString) . lookupEnv . un

maybeReadEnv :: Read a => VarName -> Env (Maybe a)
maybeReadEnv = fmap (readMaybe =<<) . lookupEnv . un

readEnvOr :: Read a => VarName -> a -> Env a
readEnvOr vn defaultValue = fromMaybe defaultValue <$> maybeReadEnv vn

getFlag :: VarName -> Env Bool
getFlag =
  let decoder = (== "true") . fmap toLower
   in parseEnv (pure . decoder)

getFlagOr :: VarName -> Bool -> Env Bool
getFlagOr vn defaultValue =
  let decoder = (== "true") . fmap toLower
   in fmap (maybe defaultValue decoder) (lookupEnv (un vn))

newtype Sensitive value = Sensitive value deriving stock (Functor, Foldable, Traversable)

instance Show (Sensitive value) where
  show _ = "Sensitive <censored>"

readPassword :: FilePath -> IO (Maybe C8.ByteString)
readPassword = fmap (viaNonEmpty head . C8.lines) . C8.readFile
