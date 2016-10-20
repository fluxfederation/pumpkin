module Types exposing (..)

import Http
import Date
import BugDetails.Types


-- Messages


type Msg
    = LoadedPatches (Result Http.Error Patches)
    | LoadedBugs (Result Http.Error Bugs)
    | ShowPatchBugs String
    | HidePatchBugs String
    | BugDetailsMsg BugDetails.Types.Msg



-- Model


type alias Model =
    { selectedPatchIds : List String
    , patches : Patches
    , bugs : Bugs
    , focusedBug : Maybe BugDetails.Types.Details
    }


type alias Patches =
    List Patch


type alias Patch =
    { id : String, name : String }


type alias Bugs =
    List BugDigest


type alias BugDigest =
    { id : String
    , patchId : String
    , message : String
    , firstOccurredAt : Date.Date
    , lastOccurredAt : Date.Date
    , occurrenceCount : Int
    }


initialModel : Model
initialModel =
    Model [] [] [] Nothing
