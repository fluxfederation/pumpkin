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
  , matchOccurrences
  , DB
  , runDB
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.List ((\\))
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.PostgreSQL.Simple
       (Connection, FromRow, In(..), Only(..), close, connectPostgreSQL)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transaction
import Network.URI (URI)
import qualified Network.URI as URI
import Types

type DB a = PGTransaction a

runDB :: DB a -> IO a
runDB action = withConnection $ \conn -> runPGTransactionIO action conn

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket (connectPostgreSQL "") close

instance FromField a => FromField (IDFor t a) where
  fromField f dat = IDFor <$> fromField f dat

instance ToField a => ToField (IDFor t a) where
  toField (IDFor i) = toField i

instance FromRow Environment

loadEnvironments :: DB [Environment]
loadEnvironments =
  query_
    " SELECT id FROM \
    \   (SELECT e.*, \
    \      ( \
    \        SELECT occurrences.occurred_at AS last_occurred_at \
    \        FROM occurrences WHERE occurrences.environment_id = e.id \
    \        ORDER BY occurred_at DESC LIMIT 1 \
    \      ) \
    \    FROM environments AS e \
    \    ORDER BY last_occurred_at DESC) AS envs"

data BugSearch = BugSearch
  { bsEnvIDs :: [EnvironmentID]
  , bsClosed :: Bool
  , bsSearch :: Maybe Text
  , bsLimit :: Int
  , bsStart :: Maybe BugID
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
expandToBugDetails [] = pure []
expandToBugDetails summaries = do
  issues <-
    query
      (Only $ In (bugID <$> summaries))
      "SELECT id, bug_id, url FROM issues WHERE bug_id IN ?"
  pure $
    (\bug -> BugDetails bug (filter ((bugID bug ==) . issueBugID) issues)) <$>
    summaries

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
    fromField f mdata >>= \s ->
      case URI.parseURI s of
        Just uri -> pure uri
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
      " INSERT INTO environments (id, created_at, updated_at) \
      \ SELECT ?, NOW(), NOW() \
      \ WHERE NOT EXISTS (SELECT 1 FROM environments WHERE id = ?)"
  void $
    execute
      (env, message, data_, occurred_at)
      " INSERT INTO occurrences \
      \  (environment_id, message, data, occurred_at, created_at, updated_at) \
      \ VALUES (?, ?, ?, ?, NOW(), NOW())"

createEventsFor :: Text -> [BugID] -> DB ()
createEventsFor _ [] = pure ()
createEventsFor eventName bugIDs =
  void $
  execute
    (eventName, In bugIDs)
    "INSERT INTO events (bug_id, name, created_at, updated_at) SELECT id, ?, NOW(), NOW() FROM bugs WHERE id IN ?"

createBugsFor :: [OccurrenceID] -> DB [BugID]
createBugsFor [] = pure []
createBugsFor occurrences = do
  results <-
    query
      (Only $ In occurrences)
      " INSERT INTO bugs (primary_occurrence_id, created_at, updated_at) \
      \ SELECT DISTINCT ON (message) o.id, NOW(), NOW() \
      \   FROM occurrences o WHERE o.id IN ? \
      \  ORDER BY message, occurred_at ASC \
      \ RETURNING bugs.id"
  newPrimaries <-
    query
      ()
      " UPDATE occurrences SET bug_id = bugs.id, updated_at = NOW() \
      \   FROM bugs WHERE occurrences.id = bugs.primary_occurrence_id AND bug_id IS NULL \
      \ RETURNING occurrences.id"
  void $ matchOccurrencesToExisting (occurrences \\ (fromOnly <$> newPrimaries))
  let newBugIDs = fromOnly <$> results
  createEventsFor "created" newBugIDs
  pure newBugIDs

matchOccurrencesToExisting :: [OccurrenceID] -> DB [OccurrenceID]
matchOccurrencesToExisting [] = pure []
matchOccurrencesToExisting ids = do
  matched <-
    query
      (Only $ In ids)
      " WITH matches AS ( \
      \   SELECT o.id AS occurrence_id, o2.bug_id \
      \   FROM occurrences o \
      \   JOIN occurrences o2 \
      \     ON o2.id IN (SELECT primary_occurrence_id FROM bugs) \
      \    AND o2.message = o.message \
      \    AND o2.bug_id IS NOT NULL \
      \   WHERE o.id IN ? \
      \ ) \
      \ UPDATE occurrences SET bug_id = matches.bug_id \
      \   FROM matches WHERE occurrences.id = matches.occurrence_id \
      \ RETURNING occurrences.id"
  pure $ fromOnly <$> matched

matchOccurrences :: Int -> DB ()
matchOccurrences limit = do
  ids <-
    query
      (Only limit)
      "SELECT id FROM occurrences WHERE bug_id IS NULL LIMIT ? FOR UPDATE SKIP LOCKED"
  matched <- matchOccurrencesToExisting (fromOnly <$> ids)
  let remaining = (fromOnly <$> ids) \\ matched
  void $ createBugsFor remaining
