{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DB
  ( newPool
  , withDB
  , Pool
  , Connection
  , checkUser
  , registerUser
  , RegistrationProblem(..)
  , problemDescription
  , Username(..)
  ) where

import Auth (User(..))
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class
import Data.Char
import Data.Pool
import Data.String
import Database.PostgreSQL.Simple
import GHC.Generics
import Servant

import Data.Text as T

newPool :: IO (Pool Connection)
newPool = createPool (connectPostgreSQL "dbname=haskellx")
                     close
                     2  -- 10 sub-pools
                     60 -- we keep a unused connection open for 60 secs
                     10 -- 100 open connections maximum per sub-pool 

withDB :: MonadIO m => Pool Connection -> (Connection -> IO a) -> m a
withDB pool f = liftIO (withResource pool f)

checkUser :: Connection -> User -> IO Bool
checkUser conn user = fmap check $
  query conn "select 1 from users where username = ? and pwhash = digest(?, 'sha256')"
             (username user, password user)

  where check [(Only 1 :: Only Int)] = True
        check                      _ = False

-- | We only accept usernames that exclusively use the following chars:
--   'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-' and '_'
--   for use in URLs.
validUsername :: Text -> Bool
validUsername uname =
  T.all (\c -> isAlphaNum c || c == '-' || c == '_') uname

newtype Username = Username { getUsername :: Text }
  deriving (Eq, Show)

instance FromText Username where
  fromText u
    | validUsername u = Just (Username u)
    | otherwise       = Nothing

data RegistrationProblem
  = InvalidUsername
  | UsernameExists
  deriving (Eq, Show)

problemDescription :: IsString s => RegistrationProblem -> s
problemDescription InvalidUsername
  = "You can only use alpha-numeric characters along with - and _"
problemDescription UsernameExists
  = "This username already exists in our database"

registerUser :: Connection -> User -> IO (Maybe RegistrationProblem)
registerUser conn user
  | validUsername (username user) = runQuery `catch` handler
  | otherwise                     = return (Just InvalidUsername)

  where handler :: SomeException -> IO (Maybe RegistrationProblem)
        handler _ = return (Just UsernameExists)

        runQuery = do
          execute conn
                  "insert into users(username, pwhash) values (?, digest(?, 'sha256'))"
                  (username user, password user)
          return Nothing
