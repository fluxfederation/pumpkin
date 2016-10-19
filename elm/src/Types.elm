module Types exposing (..)
import Http
import Date

-- Messages

type Msg = LoadedPatches (Result Http.Error Patches)
         | LoadedBugs (Result Http.Error Bugs)
         | LoadedBugDetails (Result Http.Error BugDetails)
         | ShowPatchBugs String
         | HidePatchBugs String
         | RequestBugDetails String

-- Model

type alias Model =
  { selectedPatchIds: List String
  , patches: Patches
  , bugs: Bugs
  , focusedBug: Maybe BugDetails
  }

type alias Patches = List Patch
type alias Patch = { id: String, name: String }

type alias Bugs = List BugDigest
type alias BugDigest =
  { id: String
  , patchId: String
  , message: String
  , firstOccurredAt: Date.Date
  , lastOccurredAt: Date.Date
  , occurrenceCount: Int
  }

type alias BugDetails =
  { id: String
  , patchId: String
  , message: String
  , firstOccurredAt: Date.Date
  , lastOccurredAt: Date.Date
  , stackTrace: List String
  , occurrenceCount: Int
  }

initialModel : Model
initialModel = Model [] [] [] Nothing
