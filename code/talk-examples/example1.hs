{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Text (Text, toLower, toUpper, pack)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.JQuery
import Servant.Mock
import System.Environment
import Test.QuickCheck

-- POST /endpoint[?upper]
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText, JSON] Text
                      :> Post '[PlainText, JSON] Text

type App = LogIP :> (API :<|> Raw)

api :: Proxy API
api = Proxy

answer :: Bool -> Text -> Text
answer upper str =
  if upper
    then toUpper str
    else toLower str

endpoint :: Server API -- = Bool -> Text -> EitherT ServantErr IO Text
endpoint upper body = return (answer upper body)

queryEndpoint :: Client API -- = Bool -> Text -> EitherT ServantError IO Text
queryEndpoint = client api (BaseUrl Http "localhost" 8080)

app :: Proxy App
app = Proxy

appServer :: Server App
appServer = endpoint
       :<|> serveDirectory "code/talk-examples"

runClients :: IO ()
runClients = do
  threadDelay 5000000 -- 5 seconds
  responses <- runEitherT $ do
    res1 <- queryEndpoint False "Hello"
    res2 <- queryEndpoint True  "Hello"
    return (res1, res2)
  print responses

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

data LogIP

instance HasServer api => HasServer (LogIP :> api) where
  type ServerT (LogIP :> api) m = ServerT api m

  route _ subserver = \request resp -> do
    print (remoteHost request)
    route (Proxy :: Proxy api) subserver request resp

main :: IO ()
main = do
  args <- getArgs
  forkIO runClients
  run 8080 $ case args of
    ["mock"] -> serve app (mock api :<|> serveDirectory "code/talk-examples")
    _        -> serve app appServer
