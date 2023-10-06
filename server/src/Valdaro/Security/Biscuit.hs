{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}

module Valdaro.Security.Biscuit
  ( protect
  , genServantCtx
  , generateToken
  ) where

import           Auth.Biscuit
import           Auth.Biscuit.Datalog.AST
import           Auth.Biscuit.Servant
import           Control.Monad.Error.Class        (MonadError)
import           Data.Time.Clock                  (getCurrentTime)
import           Network.Wai                      (Request)
import           Relude.Extra.Newtype
import           Servant.Server                   (Context, HasServer,
                                                   ServerError, ServerT, err401,
                                                   err403, err500, hoistServer)
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Valdaro.Configuration            (Sensitive (..))
import           Valdaro.Security.Configuration
import           Valdaro.Server.Utils             (throwJSON)

genServantCtx :: HasSecurity env => env -> Context '[AuthHandler Request (Biscuit OpenOrSealed Verified)]
genServantCtx = genBiscuitCtxWith . biscuitConfig . publicKey . un . getSecurity

biscuitConfig :: PublicKey -> BiscuitConfig String
biscuitConfig pk =
  (defaultBiscuitConfig pk)
    { onParseError = const (throwJSON err401 ("Error parsing the token." :: Text))
    , onExtractionError = throwJSON err401 . fromString @Text
    }

type HandlerLike handler = (MonadIO handler, MonadError ServerError handler)

protect :: forall (api :: Type) (handler :: Type -> Type)
         . (HasServer api '[], HandlerLike handler)
        => Proxy api
        -> ServerT api (WithAuthorizer handler)
        -> Biscuit OpenOrSealed Verified
        -> ServerT api handler
protect api server b = hoistServer api (authenticateAndCheck @handler b) server

authenticateAndCheck :: HandlerLike handler
                     => Biscuit OpenOrSealed Verified
                     -> WithAuthorizer handler a -> handler a
authenticateAndCheck biscuit =
  let nowFact = do
        now <- liftIO getCurrentTime
        pure [authorizer| time({now}); |]
   in handleBiscuitWith biscuitErrorHandler biscuit . withPriorityAuthorizerM nowFact

biscuitErrorHandler :: MonadError ServerError handler => ExecutionError -> handler result
biscuitErrorHandler = \case
  Timeout -> throwJSON err500 ("Something went wrong while checking your token. Please retry." :: Text)
  _       -> throwJSON err403 ("Invalid token." :: Text)

generateToken :: (HasSecurity env, MonadReader env m, MonadIO m, ToTerm email 'NotWithinSet 'InFact) => email -> m (Biscuit Sealed Verified)
generateToken email = do
  Security { secretKey } <- asks getSecurity
  liftIO $ seal <$>
    mkBiscuit (un secretKey)
      [block| user({email}); |]
