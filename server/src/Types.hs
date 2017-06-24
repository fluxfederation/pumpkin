{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (Value)
import Data.Time.LocalTime
import Data.UUID.Types (UUID)
import GHC.Generics

type BugID = UUID

data Bug = Bug
  { bugID :: BugID
  , bugEnvironmentID :: EnvironmentID
  , bugMessage :: String
  , bugFirstOccurredAt :: LocalTime
  , bugLastOccurredAt :: LocalTime
  , bugOccurrenceCount :: Int
  , bugClosedAt :: Maybe LocalTime
  } deriving (Generic, Show)

data BugWithIssues = BugWithIssues
  { bwiBug :: Bug
  , bwiIssues :: [Issue]
  } deriving (Generic, Show)

type EnvironmentID = String

data Environment = Environment
  { environmentID :: EnvironmentID
  } deriving (Generic, Show)

type IssueID = UUID

-- TODO: URL type?
data Issue = Issue
  { issueID :: IssueID
  , issueBugID :: BugID
  , issueURL :: String
  } deriving (Generic, Show)
