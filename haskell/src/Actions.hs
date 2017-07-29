{-# LANGUAGE OverloadedStrings #-}

module Actions where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified DB
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.URI (URI)
import Servant.Server
import Types

getEnvironments :: Handler [Environment]
getEnvironments = liftDB DB.loadEnvironments

liftDB :: DB.DB a -> Handler a
liftDB = liftIO . DB.runDB

getBugs ::
     [EnvironmentID]
  -> Bool
  -> Maybe Text
  -> Maybe Int
  -> Maybe BugID
  -> Handler [BugDetails]
getBugs envIDs closed search limit start = liftDB (DB.loadBugs bs)
  where
    bs = DB.BugSearch envIDs closed search (fromMaybe 100 limit) start

getBugDetails :: BugID -> Handler BugDetails
getBugDetails bug = do
  found <- liftDB (DB.loadBugDetails bug)
  case found of
    Nothing -> throwError err404
    Just details -> pure details

getBugOccurrences :: BugID -> Maybe Int -> Handler [Occurrence]
getBugOccurrences bug limit
                      -- should technically return 404 if bug does not exist
 = liftDB (DB.loadBugOccurrences bug (fromMaybe 100 limit))

closeBug :: BugID -> Handler BugDetails
closeBug bug = do
  liftDB $ DB.closeBug bug
  getBugDetails bug

createIssue :: BugID -> Maybe URI -> Handler BugDetails
createIssue bug url =
  case url of
    Just u -> do
      liftDB $ DB.createIssue bug u
      getBugDetails bug
    _ -> throwError err400 {errBody = "Missing URL"}

deleteIssue :: BugID -> Maybe IssueID -> Handler BugDetails
deleteIssue bug mIssueID =
  case mIssueID of
    Just issue -> do
      liftDB $ DB.deleteIssue bug issue
      getBugDetails bug
    _ -> throwError err400 {errBody = "Missing issue ID"}

createOccurrence :: NewOccurrence -> Handler ()
createOccurrence = liftDB . DB.createOccurrence
