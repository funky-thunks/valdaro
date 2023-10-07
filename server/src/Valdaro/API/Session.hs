{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Valdaro.API.Session
  ( API
  , server
  ) where

import           Auth.Biscuit.Servant
import           Control.Monad.Error.Class (throwError)
import           Servant.API
import           Servant.Server            (ServerT, err403,
                                            err404)
import           Valdaro.Server            (HandlerLike)
import           Valdaro.SQL

type API user = "user" :> "me" :> Get '[JSON] user

server :: (CanSQL r handler, HandlerLike handler) => (Text -> handler (Maybe user)) -> ServerT (API user) (WithAuthorizer handler)
server = currentUserHandler

currentUserHandler :: (CanSQL r handler, HandlerLike handler) => (Text -> handler (Maybe user)) -> WithAuthorizer handler user
currentUserHandler lookupUser =
  withTransformation extractEmail $
    withAuthorizer [authorizer|
        allow if user($email);
      |] $ maybe (throwError err404) pure =<< lift . lookupUser =<< ask

extractEmail :: HandlerLike handler => AuthorizedBiscuit OpenOrSealed -> handler Text
extractEmail AuthorizedBiscuit { authorizationSuccess } =
  let b = bindings $ matchedAllowQuery authorizationSuccess
   in maybe (throwError err403) pure $ getSingleVariableValue b "email"
