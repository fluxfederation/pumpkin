{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries
  ( loadEnvironments
  , loadBugs
  , BugSearch(..)
  , withConnection
  , Connection
  ) where

import Control.Exception (bracket)
import Data.Aeson (Value)
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

loadBugs :: BugSearch -> IO [BugWithIssues]
loadBugs search =
  withConnection $ \conn -> do
    bugs <-
      query
        conn
        " SELECT b.id \
        \      , environment_id \
        \      , message \
        \      , o.occurred_at AS first_occurred_at \
        \      , last_occurred_at \
        \      , (SELECT COUNT(1) FROM occurrences WHERE bug_id = b.id) AS occurrence_count \
        \      , e.created_at AS closed_at \
        \      , data \
        \ FROM bug_with_latest_details b \
        \ JOIN occurrences o ON o.id = b.primary_occurrence_id AND o.environment_id IN ? \
        \ LEFT OUTER JOIN events e ON latest_event_id = e.id AND e.name = 'closed' \
        \ WHERE (e.id IS NULL OR ?) \
        \   AND (? IS NULL \
        \        OR b.last_occurred_at <= \
        \           (SELECT last_occurred_at FROM bug_with_latest_details WHERE id = ?)) \
        \   AND (? IS NULL OR ? = '' \
        \        OR EXISTS (SELECT 1 FROM occurrences WHERE bug_id = b.id AND message @@ ?)) \
        \ ORDER BY last_occurred_at DESC LIMIT ?"
        ( In (bsEnvIDs search)
        , bsClosed search
        , bsStart search
        , bsStart search
        , bsSearch search
        , bsSearch search
        , bsSearch search
        , bsLimit search)
    issues <-
      query
        conn
        "SELECT id, bug_id, url FROM issues WHERE bug_id IN ?"
        (Only $ In (bugID <$> bugs))
    return $
      (\bug -> BugWithIssues bug [i | i <- issues, issueBugID i == bugID bug]) <$>
      bugs
