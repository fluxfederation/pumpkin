module BugDetails.Types exposing (..)

import Http
import Date

type Msg = LoadedDetails (Result Http.Error Details)
         | RequestDetails String

type alias Details =
  { id: String
  , patchId: String
  , message: String
  , firstOccurredAt: Date.Date
  , lastOccurredAt: Date.Date
  , stackTrace: List String
  , occurrenceCount: Int
  }
