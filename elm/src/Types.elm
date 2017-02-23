module Types exposing (..)

import Http
import Date
import Time
import String


-- Messages


type Msg
    = LoadedPatches (Result Http.Error Patches)
    | LoadedBugs (Result Http.Error Bugs)
    | ShowPatchBugs String
    | HidePatchBugs String
    | ShowClosedBugs
    | HideClosedBugs
    | LoadedDetails (Result Http.Error Bug)
    | LoadedOccurrences (Result Http.Error Occurrences)
    | RequestDetails String
    | ClosedBug (Result Http.Error Bug)
    | CloseBug String
    | HideBug
    | ClearError
    | ToggleMenu
    | ToggleFullStackTrace
    | ToggleTimeFormat
    | TimeTick Time.Time



-- Model


type alias Model =
    { selectedPatchIds : List String
    , patches : Patches
    , bugs : Bugs
    , focusedBug : Maybe Bug
    , focusedBugOccurrences : Maybe Occurrences
    , showFullStackTrace : Bool
    , error : Maybe String
    , showClosedBugs : Bool
    , showMenu : Bool
    , now : Date.Date
    , showTimeAgo : Bool
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
    , closedAt : Maybe Date.Date
    , stackTrace : Maybe (List String)
    }


type alias Occurrences =
    List Occurrence


type alias Occurrence =
    { id : String
    , patchId : String
    , message : String
    , occurredAt : Date.Date
    }


initialModel : Model
initialModel =
    { selectedPatchIds = []
    , patches = []
    , bugs = []
    , focusedBug = Nothing
    , focusedBugOccurrences = Nothing
    , showFullStackTrace = False
    , error = Nothing
    , showClosedBugs = False
    , showMenu = False
    , now = (Date.fromTime 0)
    , showTimeAgo = True
    }


isClosed : Bug -> Bool
isClosed bug =
    Maybe.withDefault False <| Maybe.map (\x -> True) bug.closedAt


stackTraceString : Bug -> String
stackTraceString bug =
    Maybe.withDefault "" <| Maybe.map (\trace -> String.join ",\n" trace) bug.stackTrace
