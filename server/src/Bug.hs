{-# LANGUAGE DeriveGeneric #-}

module Bug where

import Environment
import GHC.Generics

type BugID = String

data Bug = Bug
  { id :: BugID
  , environmentID :: EnvironmentID
  , message :: String
  -- , firstOccurredAt :: Date
  -- , lastOccurredAt :: Date
  , occurrenceCount :: Int
  -- , closedAt :: Maybe Date
  -- , issues :: [Issue]
  , stackTrace :: Maybe [String]
  } deriving (Generic)
