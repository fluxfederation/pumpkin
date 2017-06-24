{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries
  ( loadEnvironments
  , loadBugs
  , BugSearch(..)
  , loadBugDetails
  , loadBugOccurrences
  , withConnection
  , Connection
  ) where

import Control.Exception (bracket)
import Data.Aeson (Value)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Time.LocalTime
import Database.PostgreSQL.Simple
import Types

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket (connectPostgreSQL "") close

instance FromRow Environment

loadEnvironments :: IO [Environment]
loadEnvironments =
  withConnection $ \conn ->
    query_
      conn
      " SELECT id FROM \
      \   (SELECT e.*, last_occurred_at FROM environments e \
      \      JOIN (SELECT environment_id, MAX(occurred_at) AS last_occurred_at \
      \            FROM occurrences GROUP BY environment_id) AS l \
      \        ON l.environment_id = e.id \
      \     ORDER BY last_occurred_at DESC) AS envs"

data BugSearch = BugSearch
  { bsEnvIDs :: [EnvironmentID]
  , bsClosed :: Bool
  , bsSearch :: Maybe String
  , bsLimit :: Int
  , bsStart :: Maybe Int
  } deriving (Show)

instance FromRow Issue

instance FromRow Bug

bugListSelect :: Query
bugListSelect =
  " SELECT b.id \
  \      , environment_id \
  \      , message \
  \      , o.occurred_at AS first_occurred_at \
  \      , last_occurred_at \
  \      , (SELECT COUNT(1) FROM occurrences WHERE bug_id = b.id) AS occurrence_count \
  \      , e.created_at AS closed_at \
  \      , o.data \
  \ FROM bug_with_latest_details b \
  \ JOIN occurrences o ON o.id = b.primary_occurrence_id \
  \ LEFT OUTER JOIN events e ON latest_event_id = e.id AND e.name = 'closed'"

loadBugs :: BugSearch -> IO [BugWithIssues]
loadBugs search =
  withConnection $ \conn -> do
    bugs <-
      query
        conn
        ("WITH bug_list AS (" <> bugListSelect <> ") " <>
         "SELECT id, environment_id, message, first_occurred_at, \
         \       last_occurred_at, occurrence_count, closed_at \
         \  FROM bug_list \
         \ WHERE environment_id IN ? \
         \   AND (closed_at IS NULL OR ?) \
         \   AND (? IS NULL \
         \        OR last_occurred_at <= \
         \           (SELECT last_occurred_at FROM bug_with_latest_details WHERE id = ?)) \
         \   AND (? IS NULL OR ? = '' \
         \        OR EXISTS (SELECT 1 FROM occurrences \
         \                   WHERE bug_id = bug_list.id AND message @@ ?)) \
         \ ORDER BY last_occurred_at DESC LIMIT ?")
        ( In (bsEnvIDs search)
        , bsClosed search
        , bsStart search
        , bsStart search
        , bsSearch search
        , bsSearch search
        , bsSearch search
        , bsLimit search)
    issues <- loadIssuesByBugID conn (bugID <$> bugs)
    return $
      (\bug -> BugWithIssues bug [i | i <- issues, issueBugID i == bugID bug]) <$>
      bugs

loadIssuesByBugID :: Connection -> [BugID] -> IO [Issue]
loadIssuesByBugID conn ids =
  query
    conn
    "SELECT id, bug_id, url FROM issues WHERE bug_id IN ?"
    (Only $ In ids)

loadBugDetails :: BugID -> IO (Maybe BugDetails)
loadBugDetails id =
  withConnection $ \conn -> do
    bugs :: [Bug :. Only Value] <-
      query
        conn
        ("WITH bug_list AS (" <> bugListSelect <>
         ") SELECT * FROM bug_list WHERE id = ?")
        (Only id)
    case listToMaybe bugs of
      Just (bug :. Only data_) -> do
        issues <- loadIssuesByBugID conn [bugID bug]
        return $ Just (BugDetails bug issues data_)
      _ -> return Nothing

instance FromRow Occurrence

loadBugOccurrences :: BugID -> Int -> IO [Occurrence]
loadBugOccurrences id limit =
  withConnection $ \conn ->
    query
      conn
      "SELECT id, message, occurred_at, data, environment_id, bug_id FROM occurrences WHERE bug_id = ? LIMIT ?"
      (id, limit)
