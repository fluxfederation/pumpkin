{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
  ( loadEnvironments
  , loadBugs
  , BugSearch(..)
  , loadBugDetails
  , loadBugOccurrences
  , closeBug
  , createIssue
  , deleteIssue
  , createOccurrence
  , DB
  , runDB
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.PostgreSQL.Simple
       (Connection, connectPostgreSQL, close, FromRow, In(..), Only(..))
import Database.PostgreSQL.Transaction
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Network.URI (URI)
import qualified Network.URI as URI
import Types

type DB a = PGTransaction a

runDB :: DB a -> IO a
runDB action = withConnection $ \conn -> runPGTransactionIO action conn

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket (connectPostgreSQL "") close

instance FromRow Environment

loadEnvironments :: DB [Environment]
loadEnvironments =
  query_
    " SELECT id FROM \
      \   (SELECT e.*, last_occurred_at FROM environments e \
      \      JOIN (SELECT environment_id, MAX(occurred_at) AS last_occurred_at \
      \            FROM occurrences GROUP BY environment_id) AS l \
      \        ON l.environment_id = e.id \
      \     ORDER BY last_occurred_at DESC) AS envs"

data BugSearch = BugSearch
  { bsEnvIDs :: [EnvironmentID]
  , bsClosed :: Bool
  , bsSearch :: Maybe Text
  , bsLimit :: Int
  , bsStart :: Maybe Int
  } deriving (Show)

instance FromRow Issue

instance FromRow BugSummary

loadBugs :: BugSearch -> DB [BugDetails]
loadBugs search = do
  bugs <-
    query
      ( bsClosed search
      , bsStart search
      , bsStart search
      , In (bsEnvIDs search)
      , bsSearch search
      , bsSearch search
      , bsSearch search
      , bsLimit search)
      "SELECT * \
        \  FROM bug_summaries \
        \ WHERE (closed_at IS NULL OR ?) \
        \   AND (? IS NULL \
        \        OR last_occurred_at <= \
        \           (SELECT last_occurred_at FROM bug_summaries WHERE id = ?)) \
        \   AND EXISTS (SELECT 1 FROM occurrences \
        \                WHERE bug_id = bug_summaries.id \
        \                  AND environment_id IN ? \
        \                  AND (? IS NULL OR ? = '' OR message @@ ?)) \
        \ ORDER BY last_occurred_at DESC LIMIT ?"
  expandToBugDetails bugs

expandToBugDetails :: [BugSummary] -> DB [BugDetails]
expandToBugDetails [] = return []
expandToBugDetails summaries = do
  issues <-
    query
      (Only $ In (bugID <$> summaries))
      "SELECT id, bug_id, url FROM issues WHERE bug_id IN ?"
  return $
    (\bug -> BugDetails bug (filter ((bugID bug ==) . issueBugID) issues)) <$> summaries

loadBugDetails :: BugID -> DB (Maybe BugDetails)
loadBugDetails bug = do
  bugs <- query (Only bug) "SELECT * FROM bug_summaries WHERE id = ?"
  listToMaybe <$> expandToBugDetails bugs

instance FromRow Occurrence

loadBugOccurrences :: BugID -> Int -> DB [Occurrence]
loadBugOccurrences bug limit =
  query
    (bug, limit)
    "SELECT id, message, occurred_at, data, environment_id, bug_id FROM occurrences WHERE bug_id = ? LIMIT ?"

closeBug :: BugID -> DB ()
closeBug bug =
  void $
  execute
    (Only bug)
    " INSERT INTO events (bug_id, name, created_at, updated_at) \
      \ SELECT id, 'closed', NOW(), NOW() FROM bugs WHERE id = ?"

instance FromField URI where
  fromField f mdata =
    fromField f mdata >>=
    \s ->
       case URI.parseURI s of
         Just uri -> return uri
         _ -> returnError ConversionFailed f ("Invalid URI: " <> s)

instance ToField URI where
  toField u = toField $ URI.uriToString id u ""

createIssue :: BugID -> URI -> DB ()
createIssue bug url =
  void $
  execute
    (bug, url, bug)
    " INSERT INTO issues (bug_id, url, created_at, updated_at) \
      \ SELECT ?, ?, NOW(), NOW() FROM bugs WHERE id = ?"

deleteIssue :: BugID -> IssueID -> DB ()
deleteIssue bug issue =
  void $ execute (bug, issue) " DELETE FROM issues WHERE bug_id = ? AND id = ?"

createOccurrence :: NewOccurrence -> DB ()
createOccurrence (NewOccurrence env message data_ occurred_at) = do
  void $
    execute
      (env, env)
      "INSERT INTO environments (id, created_at, updated_at) SELECT ?, NOW(), NOW() WHERE NOT EXISTS (SELECT 1 FROM environments WHERE id = ?)"
  void $
    execute
      (env, message, data_, occurred_at)
      " INSERT INTO occurrences \
          \  (environment_id, message, data, occurred_at, created_at, updated_at) \
          \ VALUES ?, ?, ?, ?, NOW(), NOW()"
