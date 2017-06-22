{-# LANGUAGE DeriveGeneric #-}

module Environment where

import GHC.Generics
import Data.Time.LocalTime

type EnvironmentID = String

data Environment = Environment
  { id :: EnvironmentID
  } deriving (Generic, Show)
