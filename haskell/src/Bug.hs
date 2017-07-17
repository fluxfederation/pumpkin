{-# LANGUAGE DeriveGeneric #-}

module Bug where

import Data.Aeson (Value)
import Data.Time.LocalTime
import Data.UUID.Types (UUID)
import Environment
import GHC.Generics

type BugID = UUID

data Bug = Bug
  { id :: BugID
  , environmentID :: EnvironmentID
  , message :: String
  , firstOccurredAt :: LocalTime
  , lastOccurredAt :: LocalTime
  , occurrenceCount :: Int
  , closedAt :: Maybe LocalTime
    -- , issues :: [Issue]
  , data_ :: Value
  } deriving (Generic, Show)
