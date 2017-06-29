{-# LANGUAGE OverloadedStrings #-}

module Actions where

import Control.Monad.Except
import qualified DB
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.URI (URI)
import Servant.Server
import Types

getEnvironments :: ExceptT ServantErr IO [Environment]
getEnvironments = lift DB.loadEnvironments

getBugs
  :: [Text]
  -> Bool
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ServantErr IO [BugDetails]
getBugs envIDs closed search limit start = lift (DB.loadBugs bs)
  where
    bs = DB.BugSearch envIDs closed search (fromMaybe 100 limit) start

getBugDetails :: BugID -> ExceptT ServantErr IO BugDetails
getBugDetails bug = do
  found <- liftIO (DB.loadBugDetails bug)
  case found of
    Nothing -> throwError err404
    Just details -> return details

getBugOccurrences :: BugID -> Maybe Int -> ExceptT ServantErr IO [Occurrence]
getBugOccurrences bug limit
                      -- should technically return 404 if bug does not exist
 = liftIO (DB.loadBugOccurrences bug (fromMaybe 100 limit))

closeBug :: BugID -> ExceptT ServantErr IO BugDetails
closeBug bug = do
  liftIO $ DB.closeBug bug
  getBugDetails bug

createIssue :: BugID -> Maybe URI -> ExceptT ServantErr IO BugDetails
createIssue bug url =
  case url of
    Just u -> do
      liftIO $ DB.createIssue bug u
      getBugDetails bug
    _ ->
      throwError
        err400
        { errBody = "Missing URL"
        }

deleteIssue :: BugID -> Maybe IssueID -> ExceptT ServantErr IO BugDetails
deleteIssue bug mIssueID =
  case mIssueID of
    Just issue -> do
      liftIO $ DB.deleteIssue bug issue
      getBugDetails bug
    _ ->
      throwError
        err400
        { errBody = "Missing issue ID"
        }

createOccurrence :: NewOccurrence -> ExceptT ServantErr IO ()
createOccurrence = liftIO . DB.createOccurrence
