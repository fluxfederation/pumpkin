{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (Value)
import Data.Text
import Data.Time.LocalTime
import Data.UUID.Types (UUID)
import GHC.Generics
import Network.URI (URI)

newtype IDFor t a =
  IDFor a
  deriving (Show, Eq, Ord)

type BugID = IDFor BugSummary UUID

data BugSummary = BugSummary
  { bugID :: BugID
  , bugMessage :: Text
  , bugFirstOccurredAt :: LocalTime
  , bugLastOccurredAt :: LocalTime
  , bugClosedAt :: Maybe LocalTime
  , bugOccurrenceCount :: Int
  } deriving (Generic, Show)

data BugDetails = BugDetails
  { bwiBug :: BugSummary
  , bwiIssues :: [Issue]
  } deriving (Generic, Show)

type OccurrenceID = IDFor Occurrence UUID

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

type EnvironmentID = IDFor Environment Text

data Environment = Environment
  { environmentID :: EnvironmentID
  } deriving (Generic, Show)

type IssueID = IDFor Issue UUID

data Issue = Issue
  { issueID :: IssueID
  , issueBugID :: BugID
  , issueURL :: URI
  } deriving (Generic, Show)
