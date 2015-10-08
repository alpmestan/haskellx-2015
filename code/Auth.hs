{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Auth where

import Control.Monad
import Data.ByteString.Base64 (decodeLenient)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word8 (isSpace, toLower, _colon)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.EDE
import Servant.Server.Internal
import Text.EDE

import qualified Data.ByteString as B

data Auth

data User = User { username :: Text, password :: Text }
  deriving (Eq, Show)

instance ToObject User where
  toObject usr = fromPairs [ "username" .= username usr ]

data AuthProtected handlers = AP
  { checkCreds :: User -> IO Bool
  , onMissingAuthData :: IO Response
  , onCheckFailed :: IO Response
  , protectedHandlers :: handlers
  }

getBasicAuthData :: Request -> Maybe User
getBasicAuthData request = do
  authBs <- lookup "Authorization" (requestHeaders request)
  let (x,y) = B.break isSpace authBs
  guard (B.map toLower x == "basic")
  -- decode the base64-encoded username and password
  let (username, passWithColonAtHead) = B.break (== _colon) (decodeLenient (B.dropWhile isSpace y))
  (_, password) <- B.uncons passWithColonAtHead
  return $ User (decodeUtf8 username) (decodeUtf8 password)

instance HasServer api => HasServer (Auth :> api) where
  type ServerT (Auth :> api) m =
    AuthProtected (User -> ServerT api m)

  route _ authprotected req resp = do
    let musr = getBasicAuthData req
    case musr of
      Nothing -> onMissingAuthData authprotected >>= resp . succeedWith
      Just u  -> do
        checkOK <- checkCreds authprotected u
        if checkOK
          then route (Proxy :: Proxy api)
                     (protectedHandlers authprotected u)
                     req
                     resp
          else onCheckFailed authprotected >>= resp . succeedWith

protectWith :: (User -> IO Bool)
            -> handlers
            -> AuthProtected handlers
protectWith check hndls = AP check onMissing checkFailed hndls

  where onMissing = return authfailure
        checkFailed = return authfailure

        authfailure = responseLBS status401 [("WWW-Authenticate", headerBytes)] ""
        headerBytes = "Basic realm=\"Soundskell\""
