module Valdaro.Security.Password
  ( hashPassword
  , verifyPassword
  , PasswordHash
  ) where

import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import qualified Sel.Hashing.Password                 as Sel
import           Sel.Hashing.Password                 hiding (PasswordHash)

newtype PasswordHash = PasswordHash { getPasswordHash :: Sel.PasswordHash } deriving newtype Show

instance FromField PasswordHash where
  fromField f = fmap (PasswordHash . asciiTextToPasswordHash) . fromField f

instance ToField PasswordHash where
  toField = toField . passwordHashToText . getPasswordHash

hashPassword :: Text -> IO PasswordHash
hashPassword = fmap PasswordHash . hashText

verifyPassword :: PasswordHash -> Text -> Bool
verifyPassword = verifyText . getPasswordHash
