{-# LANGUAGE OverloadedStrings #-}

module Valdaro.Server
  ( HandlerLike
  , throwOrYield
  , throwJSON
  ) where

import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Aeson                (ToJSON, encode, object, (.=))
import           Servant.Server

type HandlerLike handler = (MonadIO handler, MonadError ServerError handler)

throwOrYield :: (HandlerLike handler, ToJSON errorType)
             => ServerError -> Either errorType successType -> handler successType
throwOrYield baseError = either (throwJSON baseError) pure

throwJSON :: (HandlerLike handler, ToJSON errorType)
          => ServerError -> errorType -> handler any
throwJSON baseError value =
  throwError $
    baseError { errBody = encode (object [ "message" .= value ])
              , errHeaders = [("Content-Type", "application/json")]
              }
