{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import qualified Data.UUID.Types as UUID
import Data.UUID.Types (UUID)
import Types

instance ToJSON UUID.UUID where
  toJSON = toJSON . UUID.toString

instance ToJSON Bug where
  toJSON b =
    object
      [ "id" .= bugID b
      , "environment_id" .= bugEnvironmentID b
      , "message" .= bugMessage b
      , "first_occurred_at" .= bugFirstOccurredAt b
      , "last_occurred_at" .= bugLastOccurredAt b
      , "occurrence_count" .= bugOccurrenceCount b
      , "closed_at" .= bugClosedAt b
      ]

mergeJSON :: Value -> Value -> Value
mergeJSON (Object a) (Object b) = Object (HM.unionWith mergeJSON a b)
mergeJSON a _b = a

instance ToJSON BugWithIssues where
  toJSON (BugWithIssues bug issues) =
    mergeJSON (toJSON bug) (object ["issues" .= issues])

instance ToJSON BugDetails where
  toJSON (BugDetails bug issues occs) =
    mergeJSON (toJSON bug) (object ["issues" .= issues, "occurrences" .= occs])

instance ToJSON Occurrence where
  toJSON occ =
    object
      [ "id" .= occID occ
      , "message" .= occMessage occ
      , "occurred_at" .= occOccurredAt occ
      , "data" .= occData occ
      , "environment_id" .= occEnvironmentID occ
      , "bug_id" .= occBugID occ
      ]

instance ToJSON Environment where
  toJSON e = object ["id" .= environmentID e]

instance ToJSON Issue where
  toJSON i =
    object ["id" .= issueID i, "bug_id" .= issueBugID i, "url" .= issueURL i]
