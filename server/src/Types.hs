{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (Value)
import Data.Text
import Data.Time.LocalTime
import Data.UUID.Types (UUID)
import GHC.Generics
import Network.URI (URI)

type BugID = UUID

data Bug = Bug
  { bugID :: BugID
  , bugEnvironmentID :: EnvironmentID
  , bugMessage :: Text
  , bugFirstOccurredAt :: LocalTime
  , bugLastOccurredAt :: LocalTime
  , bugOccurrenceCount :: Int
  , bugClosedAt :: Maybe LocalTime
  } deriving (Generic, Show)

data BugWithIssues = BugWithIssues
  { bwiBug :: Bug
  , bwiIssues :: [Issue]
  } deriving (Generic, Show)

data BugDetails = BugDetails
  { bdBug :: Bug
  , bdIssues :: [Issue]
  , bdData :: Value
  } deriving (Generic, Show)

type OccurrenceID = UUID

data Occurrence = Occurrence
  { occID :: OccurrenceID
  , occMessage :: Text
  , occOccurredAt :: LocalTime
  , occData :: Value
  , occEnvironmentID :: EnvironmentID
  , occBugID :: BugID
  } deriving (Generic, Show)

data NewOccurrence = NewOccurrence
  { neEnvironmentID :: EnvironmentID
  , neMessage :: Text
  , neData :: Value
  , neOccurredAt :: LocalTime
  } deriving (Generic, Show)

type EnvironmentID = Text

data Environment = Environment
  { environmentID :: EnvironmentID
  } deriving (Generic, Show)

type IssueID = UUID

data Issue = Issue
  { issueID :: IssueID
  , issueBugID :: BugID
  , issueURL :: URI
  } deriving (Generic, Show)
