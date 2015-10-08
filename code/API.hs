{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API where

import Auth
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Database.PostgreSQL.Simple
import DB
import Files
import GHC.Generics
import Network.Wai
import Network.Wai.Parse
import Servant
import Servant.EDE
import System.FilePath
import System.IO

import qualified Data.ByteString.Char8 as B

type API =
           -- /register
           "register" :> Get '[HTML "register.tpl"] Object -- registration form

      :<|> "register" :> ReqBody '[FormUrlEncoded] User
                      :> Post '[HTML "register_result.tpl"] Object -- registration processing


      :<|> "img" :> Raw   -- serve the image used in the header
      :<|> "songs" :> Raw -- serve the uploaded songs

      :<|> "u" :> Auth :> (
             -- /upload
             "upload" :> Get '[HTML "upload.tpl"] Object -- upload form
        :<|> "upload" :> Files Tmp :> Post '[HTML "upload_result.tpl"] User -- upload processing
        
             -- /u/:user 
        :<|> Capture "user" Username
          :> Get '[HTML "user_profile.tpl"] UserProfile -- user profile
      )

instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs =
    User <$> lkp "username" <*> lkp "password"

    where lkp name = case lookup name inputs of
            Nothing -> Left ("No value specified for " ++ unpack name)
            Just v  -> Right v

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server pool = return mempty
         :<|> register pool
         :<|> serveDirectory "img"
         :<|> serveDirectory "songs"
         :<|> protectWith (\u -> withDB pool (\conn -> checkUser conn u))
                          (\u -> return mempty
                            :<|> uploadSong pool u
                            :<|> (\uname -> getUserProfile pool uname u)
                          )

data UserProfile = UserProfile
  { uname     :: Text
  , joined_on :: Text
  , songs     :: [Song]
  , itsMe     :: Bool
  } deriving Generic

instance ToObject UserProfile

data Song = Song
  { songTitle    :: Text
  , songFilepath :: FilePath
  , songAddedOn  :: Text
  } deriving Generic

instance ToJSON Song

getUserProfile :: Pool Connection -> Username -> User -> EitherT ServantErr IO UserProfile
getUserProfile pool uname user = do
  mprofile <- withDB pool $ \conn -> do
    userData <- fmap (fmap fromOnly . listToMaybe) $
      query conn "select to_char(joined_on, 'YYYY-MM-DD') \
                 \ from users where username = ?"
                 (Only $ getUsername uname)
    songs <- fmap (map $ \(t, fp, added_on) -> Song t (takeFileName fp) added_on) $
      query conn "select s.title, s.filepath, to_char(s.added_on, 'YYYY-MM-DD') \
                        \ from songs s \
                        \ where s.username = ? \
                        \ order by s.added_on desc"
                        (Only $ getUsername uname)
    return $
      UserProfile (getUsername uname)
              <$> userData
              <*> pure songs
              <*> pure (getUsername uname == username user)

  maybe errorOut return mprofile
  
  where errorOut = left $ err404 { errBody = "This user doesn't exist" }


uploadSong :: Pool Connection -> User -> MultiPartData Tmp -> EitherT ServantErr IO User
uploadSong pool user ([("song_title", title)], [("song_file", filedata)]) = do
  successful <- liftIO $ do
    (fp, hndl) <- openTempFile "./songs" ("song" <.> takeExtension (B.unpack $ fileName filedata))
    B.hPutStr hndl =<< B.readFile (fileContent filedata)
    hClose hndl

    withDB pool $ \conn -> (do
      execute conn "insert into songs(title, filepath, username) values (?, ?, ?)"
                   (decodeUtf8 title, fp, username user)
      return True)
        `catch` handler

  if successful
    then return user
    else left $ err400 { errBody = "Couldn't add the song to the database" }

  where handler :: SomeException -> IO Bool
        handler _ = return False
 
uploadSong _ _ _ = left $ err400 { errBody = "Invalid form data" }


register :: Pool Connection -> User -> EitherT ServantErr IO Object
register pool usr = do
  mproblem <- withDB pool (\conn -> registerUser conn usr)
  maybe (return mempty) problemToError mproblem

  where problemToError pb = left $ err400 { errBody = problemDescription pb }

app :: Pool Connection -> Application
app pool = serve api (server pool)
