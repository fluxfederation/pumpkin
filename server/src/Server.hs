{-# LANGUAGE OverloadedStrings #-}

module Server
  ( runServer
  ) where

import API
import Control.Monad.Except
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.UUID.Types as UUID
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
  -> ExceptT ServantErr IO [Bug]
getBugs envIDs closed search limit start = lift (Queries.loadBugs bs)
  where
    bs = BugSearch envIDs closed search (fromMaybe 100 limit) start

api :: Server API
api = getEnvironments :<|> getBugs

instance ToJSON UUID.UUID where
  toJSON = toJSON . UUID.toString

instance ToJSON Bug where
  toJSON b =
    object
      [ "id" .= bugID b
      , "environment_id" .= bugEnvironmentID b
      , "message" .= bugMessage b
      , "first_occurred_at" .= bugFirstOccurredAt b
      , "last_occurred_at" .= bugLastOccurredAt b
      , "occurrence_count" .= bugOccurrenceCount b
      , "closed_at" .= bugClosedAt b
      , "data" .= bugData b
      , "issues" .= bugIssues b
      ]

instance ToJSON Environment where
  toJSON e = object ["id" .= environmentID e]

instance ToJSON Issue where
  toJSON i =
    object ["id" .= issueID i, "bug_id" .= issueBugID i, "url" .= issueURL i]

apiAPP :: Application
apiAPP = serve (Proxy :: Proxy API) api

runServer :: FilePath -> IO ()
runServer root = do
  changeWorkingDirectory root
  Warp.run 8080 (static apiAPP)
