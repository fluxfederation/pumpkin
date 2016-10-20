module Types exposing (..)

import Http
import Date


-- Messages


type Msg
    = LoadedPatches (Result Http.Error Patches)
    | LoadedBugs (Result Http.Error Bugs)
    | ShowPatchBugs String
    | HidePatchBugs String
    | LoadedDetails (Result Http.Error Bug)
    | RequestDetails String
    | ClosedBug (Result Http.Error Bug)
    | CloseBug String



-- Model


type alias Model =
    { selectedPatchIds : List String
    , patches : Patches
    , bugs : Bugs
    , focusedBug : Maybe Bug
    }


type alias Event =
    { name : String }


type alias Patches =
    List Patch


type alias Patch =
    { id : String, name : String }


type alias Bugs =
    List Bug


type alias Bug =
    { id : String
    , patchId : String
    , message : String
    , firstOccurredAt : Date.Date
    , lastOccurredAt : Date.Date
    , occurrenceCount : Int
    , latestEvent : Event
    , stackTrace : Maybe (List String)
    }


initialModel : Model
initialModel =
    Model [] [] [] Nothing
