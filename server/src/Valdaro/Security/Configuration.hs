{-# LANGUAGE OverloadedStrings #-}

module Valdaro.Security.Configuration
  ( readConfiguration
  , Configuration
  , buildSecurity
  , Security(..)
  , HasSecurity(..)
  ) where

import           Auth.Biscuit
import           Valdaro.Configuration (Env, Sensitive (..), getEnv,
                                        readPassword)

newtype Configuration =
  Configuration
    { secretKeyFile :: FilePath
    }

readConfiguration :: Env Configuration
readConfiguration = Configuration <$> getEnv "BISCUIT_SECRET_FILE"

buildSecurity :: Configuration -> IO (Maybe Security)
buildSecurity Configuration { secretKeyFile } = do
  secretKey <- readPassword secretKeyFile
  pure $ (Security <$> Sensitive <*> toPublic) <$> (parseSecretKeyHex =<< secretKey)

data Security =
  Security
    { secretKey :: Sensitive SecretKey
    , publicKey :: PublicKey
    } deriving stock Show

class HasSecurity env where
  getSecurity :: env -> Security

instance HasSecurity Security where
  getSecurity = id
