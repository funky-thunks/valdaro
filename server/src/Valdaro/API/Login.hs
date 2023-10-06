{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Valdaro.API.Login
  ( API
  , server
  , CanLogin
  ) where

import           Auth.Biscuit              (serializeB64)
import           Auth.Biscuit.Datalog.AST  (IsWithinSet (NotWithinSet),
                                            PredicateOrFact (InFact), ToTerm)
import           Control.Monad.Error.Class (MonadError)
import           Data.Aeson                (ToJSON (..), Value (String))
import           Servant.API
import           Servant.Server            (ServerError, ServerT, err403)
import           Valdaro.Security          (HasSecurity, PasswordHash,
                                            generateToken, verifyPassword)
import           Valdaro.Server.Utils      (throwOrYield)
import           Web.FormUrlEncoded        (FromForm)

type API email = ReqBody '[FormUrlEncoded] (LoginBody email) :> Post '[JSON] LoginResult

data LoginBody email =
  LoginBody
    { email    :: email
    , password :: Password
    } deriving stock    (Generic)

deriving anyclass instance FromHttpApiData email => FromForm (LoginBody email)

newtype Password =
  Password
    { getPassword :: Text
    } deriving newtype (FromHttpApiData)

newtype LoginResult =
  LoginResult
    { token :: Text
    } deriving stock    (Generic)
      deriving anyclass (ToJSON)

data LoginFailed = LoginFailed

instance ToJSON LoginFailed where
  toJSON LoginFailed = String "Login failed."

type CanLogin env handler email = (MonadIO handler, MonadError ServerError handler, HasSecurity env, MonadReader env handler, ToTerm email 'NotWithinSet 'InFact)

loginHandler :: (CanLogin env handler email) => (email -> handler (Maybe PasswordHash)) -> LoginBody email -> handler LoginResult
loginHandler lookupHash LoginBody { email, password } = do
  hash' <- lookupHash email
  throwOrYield err403 =<<
    case hash' of
      Nothing -> pure $ Left LoginFailed
      Just hp -> do
        if verifyPassword hp (getPassword password)
        then
          pure . LoginResult . decodeUtf8 . serializeB64 <$> generateToken email
        else
          pure $ Left LoginFailed

server :: CanLogin env handler email => (email -> handler (Maybe PasswordHash)) -> ServerT (API email) handler
server = loginHandler
