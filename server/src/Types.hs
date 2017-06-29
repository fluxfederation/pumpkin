{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (Value)
import Data.Text
import Data.Time.LocalTime
import Data.UUID.Types (UUID)
import GHC.Generics
import Network.URI (URI)

newtype BugID =
  BugID UUID
  deriving (Show, Eq, Ord)

data BugSummary = BugSummary
  { bugID :: BugID
  , bugMessage :: Text
  , bugLatestEventName :: Text
  , bugLatestEventAt :: LocalTime
  , bugFirstOccurredAt :: LocalTime
  , bugLastOccurredAt :: LocalTime
  , bugClosedAt :: Maybe LocalTime
  , bugOccurrenceCount :: Int
  } deriving (Generic, Show)

data BugDetails = BugDetails
  { bwiBug :: BugSummary
  , bwiIssues :: [Issue]
  } deriving (Generic, Show)

newtype OccurrenceID =
  OccurrenceID UUID
  deriving (Show, Eq, Ord)

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

newtype EnvironmentID =
  EnvironmentID Text
  deriving (Show, Eq, Ord)

data Environment = Environment
  { environmentID :: EnvironmentID
  } deriving (Generic, Show)

newtype IssueID =
  IssueID UUID
  deriving (Show, Eq, Ord)

data Issue = Issue
  { issueID :: IssueID
  , issueBugID :: BugID
  , issueURL :: URI
  } deriving (Generic, Show)
