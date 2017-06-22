{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries
  ( loadEnvironments
  ) where

import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Data.Time.LocalTime
import Environment

environmentsByRecencyQuery =
  "SELECT id FROM (SELECT e.*, last_occurred_at FROM environments e " <>
  "JOIN (SELECT environment_id, MAX(occurred_at) AS last_occurred_at " <>
  "      FROM occurrences GROUP BY environment_id) l " <>
  "  ON l.environment_id = e.id " <>
  "ORDER BY last_occurred_at DESC) AS envs"

instance FromRow Environment

loadEnvironments :: IO [Environment]
loadEnvironments = do
  conn <- connectPostgreSQL ""
  query_ conn environmentsByRecencyQuery
