import API
import DB
import Network.Wai.Handler.Warp
import Servant.EDE

main :: IO ()
main = do
  errs <- loadTemplates api "ui/"
  case null errs of
    False -> mapM_ print errs
    True  -> do
      pool <- newPool
      run 8080 (app pool)

