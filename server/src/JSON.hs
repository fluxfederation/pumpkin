{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import qualified Data.UUID.Types as UUID
import Data.UUID.Types (UUID)
import Network.URI (URI)
import qualified Network.URI as URI
import Types

instance ToJSON UUID where
  toJSON = toJSON . UUID.toString

instance ToJSON a =>
         ToJSON (IDFor t a) where
  toJSON (IDFor i) = toJSON i

instance FromJSON a =>
         FromJSON (IDFor t a) where
  parseJSON = fmap IDFor . parseJSON

instance ToJSON URI where
  toJSON u = toJSON $ URI.uriToString id u ""

instance ToJSON BugSummary where
  toJSON b =
    object
      [ "id" .= bugID b
      , "message" .= bugMessage b
      , "first_occurred_at" .= bugFirstOccurredAt b
      , "last_occurred_at" .= bugLastOccurredAt b
      , "occurrence_count" .= bugOccurrenceCount b
      , "closed_at" .= bugClosedAt b
      ]

mergeJSON :: Value -> Value -> Value
mergeJSON (Object a) (Object b) = Object (HM.unionWith mergeJSON a b)
mergeJSON a _b = a

instance ToJSON BugDetails where
  toJSON (BugDetails bug issues) =
    mergeJSON (toJSON bug) (object ["issues" .= issues])

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

instance FromJSON NewOccurrence where
  parseJSON (Object o) =
    NewOccurrence <$> o .: "environment" <*> o .: "message" <*> o .: "data" <*>
    o .: "occurred_at"
  parseJSON invalid = typeMismatch "Coord" invalid
