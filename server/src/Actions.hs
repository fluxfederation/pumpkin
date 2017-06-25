{-# LANGUAGE OverloadedStrings #-}

module Actions where

import Control.Monad.Except
import qualified DB
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.URI (URI)
import qualified Network.URI as URI
import Servant.Server
import Types

getEnvironments :: ExceptT ServantErr IO [Environment]
getEnvironments = lift DB.loadEnvironments

getBugs ::
     [Text]
  -> Bool
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ServantErr IO [BugWithIssues]
getBugs envIDs closed search limit start = lift (DB.loadBugs bs)
  where
    bs = DB.BugSearch envIDs closed search (fromMaybe 100 limit) start

getBugDetails :: BugID -> ExceptT ServantErr IO BugDetails
getBugDetails id = do
  found <- liftIO (DB.loadBugDetails id)
  case found of
    Nothing -> throwError err404
    Just details -> return details

getBugOccurrences :: BugID -> Maybe Int -> ExceptT ServantErr IO [Occurrence]
getBugOccurrences id limit
   -- should technically return 404 if bug does not exist
 = liftIO (DB.loadBugOccurrences id (fromMaybe 100 limit))

closeBug :: BugID -> ExceptT ServantErr IO BugDetails
closeBug id = do
  liftIO $ DB.closeBug id
  getBugDetails id

createIssue :: BugID -> Maybe URI -> ExceptT ServantErr IO BugDetails
createIssue id url =
  case url of
    Just u -> do
      liftIO $ DB.createIssue id u
      getBugDetails id
    _ -> throwError err400 {errBody = "Missing URL"}

deleteIssue :: BugID -> Maybe IssueID -> ExceptT ServantErr IO BugDetails
deleteIssue bugID mIssueID =
  case mIssueID of
    Just issueID -> do
      liftIO $ DB.deleteIssue bugID issueID
      getBugDetails bugID
    _ -> throwError err400 {errBody = "Missing issue ID"}

createOccurrence :: NewOccurrence -> ExceptT ServantErr IO ()
createOccurrence = liftIO . DB.createOccurrence
