module Server
  ( runServer
  ) where

import API
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import JSON
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Static (static)
import Queries
import Servant (Proxy(..))
import Servant.API
import Servant.Server
import System.Posix.Directory (changeWorkingDirectory)
import Types

getEnvironments :: ExceptT ServantErr IO [Environment]
getEnvironments = lift Queries.loadEnvironments

getBugs ::
     [String]
  -> Bool
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ServantErr IO [BugWithIssues]
getBugs envIDs closed search limit start = lift (Queries.loadBugs bs)
  where
    bs = BugSearch envIDs closed search (fromMaybe 100 limit) start

getBugDetails :: BugID -> ExceptT ServantErr IO BugDetails
getBugDetails id = do
  found <- liftIO (Queries.loadBugDetails id)
  case found of
    Nothing -> throwError err404
    Just details -> return details

getBugOccurrences :: BugID -> Maybe Int -> ExceptT ServantErr IO [Occurrence]
getBugOccurrences id limit
   -- should technically return 404 if bug does not exist
 = liftIO (Queries.loadBugOccurrences id (fromMaybe 100 limit))

api :: Server API
api = getEnvironments :<|> getBugs :<|> getBugDetails :<|> getBugOccurrences

apiAPP :: Application
apiAPP = serve (Proxy :: Proxy API) api

app :: Application
app req respond =
  if null (pathInfo req) -- Root page
    then respond (responseFile status200 [] "index.html" Nothing)
    else (static apiAPP) req respond

runServer :: FilePath -> IO ()
runServer root = do
  changeWorkingDirectory root
  Warp.run 8080 app
