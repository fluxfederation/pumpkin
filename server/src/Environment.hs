{-# LANGUAGE DeriveGeneric #-}

module Environment where

import GHC.Generics

type EnvironmentID = String

newtype Environment = Environment
  { id :: EnvironmentID
  } deriving (Generic)
